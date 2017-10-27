//
//  CRConnection.m
//  Criollo
//
//  Created by Cătălin Stan on 10/23/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRConnection.h"
#import "CRConnection_Internal.h"
#import "CRApplication.h"
#import "CRServer.h"
#import "CRServer_Internal.h"
#import "CRServerConfiguration.h"
#import "GCDAsyncSocket.h"
#import "CRRequest.h"
#import "CRRequest_Internal.h"
#import "CRResponse.h"
#import "CRResponse_Internal.h"

#include <sys/types.h>
#include <sys/sysctl.h>
#import "NSDate+RFC1123.h"

#define CRConnectionSocketTagSendingResponse                        20

NS_ASSUME_NONNULL_BEGIN
@interface CRConnection () <GCDAsyncSocketDelegate>

- (void)bufferBodyData:(NSData *)data forRequest:(CRRequest *)request;
- (void)bufferResponseData:(NSData *)data forRequest:(CRRequest *)request;

@end
NS_ASSUME_NONNULL_END

@implementation CRConnection

static const NSData * CRLFData;
static const NSData * CRLFCRLFData;

+ (void)initialize {
    CRLFData = [NSData dataWithBytes:"\x0D\x0A" length:2];
    CRLFCRLFData = [NSData dataWithBytes:"\x0D\x0A\x0D\x0A" length:4];
}

+ (NSData *)CRLFCRLFData {
    return (NSData *)CRLFCRLFData;
}

+ (NSData *)CRLFData {
    return (NSData *)CRLFData;
}

#pragma mark - Responses

- (CRResponse *)responseWithHTTPStatusCode:(NSUInteger)HTTPStatusCode {
    return [self responseWithHTTPStatusCode:HTTPStatusCode description:nil version:CRHTTPVersion1_1];
}

- (CRResponse *)responseWithHTTPStatusCode:(NSUInteger)HTTPStatusCode description:(NSString *)description {
    return [self responseWithHTTPStatusCode:HTTPStatusCode description:description version:CRHTTPVersion1_1];
}

- (CRResponse *)responseWithHTTPStatusCode:(NSUInteger)HTTPStatusCode description:(NSString *)description version:(CRHTTPVersion)version {
    return [[CRResponse alloc] initWithConnection:self HTTPStatusCode:HTTPStatusCode description:description version:version];
}

#pragma mark - Initializers

- (instancetype)init {
    return [self initWithSocket:nil server:nil];
}

- (instancetype)initWithSocket:(GCDAsyncSocket *)socket server:(CRServer *)server {
    self = [super init];
    if (self != nil) {
        if ( server ) {
            self.server = server;
        }
        if ( socket ) {
            self.socket = socket;
        }
        self.socket.delegate = self;
        self.requests = [NSMutableArray array];

        _remoteAddress = self.socket.connectedHost;
        _remotePort = self.socket.connectedPort;
        _localAddress = self.socket.localHost;
        _localPort = self.socket.localPort;

        _isolationQueue = dispatch_queue_create([[[NSBundle mainBundle].bundleIdentifier stringByAppendingPathExtension:[NSString stringWithFormat:@"CRConnection-IsolationQueue-%lu", (unsigned long)self.hash]] cStringUsingEncoding:NSASCIIStringEncoding], DISPATCH_QUEUE_SERIAL);
        dispatch_set_target_queue(self.isolationQueue, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0));
    }
    return self;
}

- (void)dealloc {
    _socket = nil;
    _currentRequest = nil;
    _requests = nil;
    _isolationQueue = nil;
}

#pragma mark - Data

- (void)startReading {
    self.currentRequest = nil;
}

- (void)didReceiveCompleteRequestHeaders {
    if (self.willDisconnect) {
        return;
    }
}

- (void)didReceiveRequestBodyData:(NSData *)data {
    if ( self.willDisconnect ) {
        return;
    }

    NSString * contentType = self.currentRequest.env[@"HTTP_CONTENT_TYPE"];
    if ([contentType hasPrefix:CRRequestTypeMultipart]) {
        NSError* multipartParsingError;
        if ( ![self.currentRequest parseMultipartBodyDataChunk:data error:&multipartParsingError] ) {
            [CRApp logErrorFormat:@"%@" , multipartParsingError];
        }
    } else if ([contentType hasPrefix:CRRequestTypeJSON]) {
        // JSON requests are parsed after we have all the data
        [self bufferBodyData:data forRequest:self.currentRequest];
    } else if ([contentType hasPrefix:CRRequestTypeURLEncoded]) {
        // URL-encoded requests are parsed after we have all the data
        [self bufferBodyData:data forRequest:self.currentRequest];
    } else {
        NSError* mimeParsingError;
        if ( ![self.currentRequest parseMIMEBodyDataChunk:data error:&mimeParsingError] ) {
            [CRApp logErrorFormat:@"%@" , mimeParsingError];
        }
    }
}

- (void)didReceiveCompleteRequest {
    if ( self.willDisconnect ) {
        return;
    }

    // Parse request body
    NSUInteger contentLength = [self.currentRequest.env[@"HTTP_CONTENT_LENGTH"] integerValue];
    if ( contentLength > 0 ) {
        NSError* bodyParsingError;
        NSString* contentType = self.currentRequest.env[@"HTTP_CONTENT_TYPE"];

        BOOL result = YES;

        if ([contentType hasPrefix:CRRequestTypeJSON]) {
            result = [self.currentRequest parseJSONBodyData:&bodyParsingError];
        } else if ([contentType hasPrefix:CRRequestTypeURLEncoded]) {
            result = [self.currentRequest parseURLEncodedBodyData:&bodyParsingError];
        } else if ([contentType hasPrefix:CRRequestTypeMultipart]) {
            // multipart/form-data requests are parsed as they come in and not once the
            // request hast been fully received ;)
        } else {
            // other mime types are assumed to be files and will be treated just like
            // multipart request files. What we need to do here is to reset the target
            [self.currentRequest clearBodyParsingTargets];
        }

        if ( !result ) {
            [CRApp logErrorFormat:@"%@" , bodyParsingError];
        }
    }

    CRResponse* response = [self responseWithHTTPStatusCode:200];
    self.currentRequest.response = response;
    response.request = self.currentRequest;
    [self.delegate connection:self didReceiveRequest:self.currentRequest response:response];
    [self startReading];
}

- (void)bufferBodyData:(NSData *)data forRequest:(CRRequest *)request {
    if ( self.willDisconnect ) {
        return;
    }

    [request bufferBodyData:data];
}

- (void)bufferResponseData:(NSData *)data forRequest:(CRRequest *)request {
    if ( self.willDisconnect ) {
        return;
    }

    [request bufferResponseData:data];
}

- (void)sendDataToSocket:(NSData *)data forRequest:(CRRequest *)request {
    if ( self.willDisconnect ) {
        return;
    }
    
    CRRequest* firstRequest = self.requests.firstObject;
    if ( [firstRequest isEqual:request] || self.requests.count == 0 ) {
        request.bufferedResponseData = nil;
        [self.socket writeData:data withTimeout:self.server.configuration.CRConnectionWriteTimeout tag:CRConnectionSocketTagSendingResponse];
        if ( request.shouldCloseConnection ) {
            _willDisconnect = YES;
            [self.socket disconnectAfterWriting];
        }
        if ( request.response.finished ) {
            [self didFinishResponseForRequest:request];
        }
    } else {
        [self bufferResponseData:data forRequest:request];
    }
}

- (void)didFinishResponseForRequest:(CRRequest *)request {
    CRConnection * __weak connection = self;
    [self.delegate connection:self didFinishRequest:request response:request.response];
    dispatch_async(self.isolationQueue, ^{
        [connection.requests removeObject:request];
    });
}

#pragma mark - State

- (BOOL)shouldClose {
    return NO;
}

#pragma mark - GCDAsyncSocketDelegate

- (void)socket:(GCDAsyncSocket *)sock didWriteDataWithTag:(long)tag {
    CRConnection * __weak connection = self;
    switch (tag) {
        case CRConnectionSocketTagSendingResponse: {
            dispatch_async(self.isolationQueue, ^{ @autoreleasepool {
                if ( connection.requests.count > 0 && !self.willDisconnect ) {
                    CRRequest* request = connection.requests.firstObject;
                    if ( request.bufferedResponseData.length > 0 ) {
                        dispatch_async(sock.delegateQueue, ^{
                            [connection sendDataToSocket:request.bufferedResponseData forRequest:request];
                        });
                    }
                }
            }});
        } break;

        default:
            break;
    }
}

- (void)socketDidDisconnect:(GCDAsyncSocket *)sock withError:(NSError *)err {
    [self.server didCloseConnection:self];
}

@end
