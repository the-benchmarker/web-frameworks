//
//  CRHTTPConnection.m
//  Criollo
//
//  Created by Cătălin Stan on 10/25/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRHTTPConnection.h"
#import "CRConnection_Internal.h"
#import "GCDAsyncSocket.h"
#import "CRApplication.h"
#import "CRHTTPServer.h"
#import "CRServer_Internal.h"
#import "CRHTTPServerConfiguration.h"
#import "CRMessage.h"
#import "CRMessage_Internal.h"
#import "CRRequest.h"
#import "CRRequest_Internal.h"
#import "CRResponse_Internal.h"
#import "CRHTTPResponse.h"

@interface CRHTTPConnection () {
    NSUInteger requestBodyLength;
    NSUInteger requestBodyReceivedBytesLength;
    BOOL didPerformInitialRead;
}

@end

@implementation CRHTTPConnection

#pragma mark - Data

- (void)startReading {
    [super startReading];

    requestBodyLength = 0;
    requestBodyReceivedBytesLength = 0;

    CRHTTPServerConfiguration* config = (CRHTTPServerConfiguration*)self.server.configuration;

    // Read the request headers
    NSUInteger timeout = didPerformInitialRead ? config.CRConnectionKeepAliveTimeout : config.CRConnectionReadTimeout + config.CRHTTPConnectionReadHeaderTimeout;
    [self.socket readDataToData:[CRConnection CRLFCRLFData] withTimeout:timeout maxLength:config.CRRequestMaxHeaderLength tag:CRHTTPConnectionSocketTagBeginReadingRequest];
}

- (void)didReceiveCompleteRequestHeaders {
    // Create ENV from HTTP headers
    NSMutableDictionary* env = [NSMutableDictionary dictionary];
    [self.currentRequest.allHTTPHeaderFields enumerateKeysAndObjectsUsingBlock:^(NSString * _Nonnull key, NSString * _Nonnull obj, BOOL * _Nonnull stop) {
        @autoreleasepool {
            NSString* headerName = [@"HTTP_" stringByAppendingString:[key.uppercaseString stringByReplacingOccurrencesOfString:@"-" withString:@"_"]];
            [env setObject:obj forKey:headerName];
        }
    }];

    if ( env[@"HTTP_CONTENT_LENGTH"] ) {
        env[@"CONTENT_LENGTH"] = env[@"HTTP_CONTENT_LENGTH"];
    }
    if ( env[@"HTTP_CONTENT_TYPE"] ) {
        env[@"CONTENT_TYPE"] = env[@"HTTP_CONTENT_TYPE"];
    }

    if ( env[@"HTTP_HOST"]) {
        env[@"SERVER_NAME"] = env[@"HTTP_HOST"];
    }

//    env[@"SERVER_SOFTWARE"] = @"";
    env[@"REQUEST_METHOD"] = NSStringFromCRHTTPMethod(self.currentRequest.method);
    env[@"SERVER_PROTOCOL"] = NSStringFromCRHTTPVersion(self.currentRequest.version);
    env[@"REQUEST_URI"] = self.currentRequest.URL.absoluteString;
    env[@"DOCUMENT_URI"] = self.currentRequest.URL.path;
    env[@"SCRIPT_NAME"] = self.currentRequest.URL.path;
    env[@"QUERY_STRING"] = self.currentRequest.URL.query;
    env[@"REMOTE_ADDR"] = self.socket.connectedHost;
    env[@"REMOTE_PORT"] = @(self.socket.connectedPort);
    env[@"SERVER_ADDR"] = self.socket.localHost;
    env[@"SERVER_PORT"] = @(self.socket.localPort);
    [self.currentRequest setEnv:env];

    [super didReceiveCompleteRequestHeaders];

    if ( self.willDisconnect ) {
        return;
    }

    CRHTTPServerConfiguration* config = (CRHTTPServerConfiguration*)self.server.configuration;
    requestBodyLength = [self.currentRequest valueForHTTPHeaderField:@"Content-Length"].integerValue;
    if ( requestBodyLength > 0 ) {
        NSUInteger bytesToRead = requestBodyLength < config.CRRequestBodyBufferSize ? requestBodyLength : config.CRRequestBodyBufferSize;
        [self.socket readDataToLength:bytesToRead withTimeout:config.CRHTTPConnectionReadBodyTimeout tag:CRHTTPConnectionSocketTagReadingRequestBody];
    } else {
        [self didReceiveCompleteRequest];
    }
}

#pragma mark - Responses

- (CRResponse *)responseWithHTTPStatusCode:(NSUInteger)HTTPStatusCode description:(NSString *)description version:(CRHTTPVersion)version {
    return [[CRHTTPResponse alloc] initWithConnection:self HTTPStatusCode:HTTPStatusCode description:description version:version];
}

#pragma mark - GCDAsyncSocketDelegate

- (void)socket:(GCDAsyncSocket *)sock didReadData:(NSData*)data withTag:(long)tag {

    didPerformInitialRead = YES;
    CRHTTPServerConfiguration* config = (CRHTTPServerConfiguration*)self.server.configuration;

    if ( tag == CRHTTPConnectionSocketTagBeginReadingRequest ) {

        NSData* spaceData = [@" " dataUsingEncoding:NSUTF8StringEncoding];
        BOOL result = YES;

        NSRange rangeOfFirstNewline = [data rangeOfData:[CRConnection CRLFData] options:0 range:NSMakeRange(0, data.length)];
        NSRange rangeOfFirstSpace = [data rangeOfData:spaceData options:0 range:NSMakeRange(0, rangeOfFirstNewline.location)];
        if (rangeOfFirstSpace.location != NSNotFound ) {

            NSRange methodRange = NSMakeRange(0, rangeOfFirstSpace.location);
            NSRange pathSearchRange = NSMakeRange(rangeOfFirstSpace.location + rangeOfFirstSpace.length, rangeOfFirstNewline.location - rangeOfFirstSpace.location - rangeOfFirstSpace.length);
            NSRange rangeOfSecondSpace = [data rangeOfData:spaceData options:0 range:pathSearchRange];

            if ( rangeOfSecondSpace.location != NSNotFound ) {
                NSRange pathRange = NSMakeRange(pathSearchRange.location, rangeOfSecondSpace.location - pathSearchRange.location);
                NSRange versionRange = NSMakeRange(rangeOfSecondSpace.location + rangeOfSecondSpace.length, rangeOfFirstNewline.location - rangeOfSecondSpace.location - rangeOfSecondSpace.length);

                NSString * methodSpec = [[NSString alloc] initWithBytesNoCopy:(void *)data.bytes + methodRange.location length:methodRange.length encoding:NSUTF8StringEncoding freeWhenDone:NO];
                CRHTTPMethod requestMethod = CRHTTPMethodMake(methodSpec);

                if ( requestMethod != CRHTTPMethodNone ) {
                    NSString* pathSpec = [[NSString alloc] initWithBytesNoCopy:(void *)data.bytes + pathRange.location length:pathRange.length encoding:NSUTF8StringEncoding freeWhenDone:NO];

                    NSString* versionSpec = [[NSString alloc] initWithBytesNoCopy:(void *)data.bytes + versionRange.location length:versionRange.length encoding:NSUTF8StringEncoding freeWhenDone:NO];
                    CRHTTPVersion version = CRHTTPVersionMake(versionSpec);

                    NSRange rangeOfHostHeader = [data rangeOfData:[@"Host: " dataUsingEncoding:NSUTF8StringEncoding] options:0 range:NSMakeRange(0, data.length)];

                    if ( rangeOfHostHeader.location != NSNotFound || version == CRHTTPVersion1_0 ) {
                        NSRange rangeOfNewLineAfterHost = [data rangeOfData:[CRConnection CRLFData] options:0 range:NSMakeRange(rangeOfHostHeader.location + rangeOfHostHeader.length, data.length - rangeOfHostHeader.location - rangeOfHostHeader.length)];

                        if ( rangeOfNewLineAfterHost.location == NSNotFound ) {
                            rangeOfNewLineAfterHost.location = data.length - 1;
                        }

                        NSRange hostSpecRange = NSMakeRange(rangeOfHostHeader.location + rangeOfHostHeader.length, rangeOfNewLineAfterHost.location - rangeOfHostHeader.location - rangeOfHostHeader.length);
                        NSString* hostSpec = [[NSString alloc] initWithBytesNoCopy:(void *)data.bytes + hostSpecRange.location length:hostSpecRange.length encoding:NSUTF8StringEncoding freeWhenDone:NO];

                        // TODO: request.URL should be parsed using no memcpy and using the actual scheme
#if TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR
                        NSURL* URL = [NSURL URLWithString:[NSString stringWithFormat:@"http://%@%@", hostSpec, pathSpec]];
#else
                        NSURL* URL = [NSURL URLWithString:[NSString stringWithFormat:@"http%@://%@%@", ((CRHTTPServer *)self.server).isSecure ? @"s" : @"", hostSpec, pathSpec]];
#endif
                        CRRequest* request = [[CRRequest alloc] initWithMethod:CRHTTPMethodMake(methodSpec) URL:URL version:CRHTTPVersionMake(versionSpec) connection:self];
                        CRHTTPConnection * __weak connection = self;
                        dispatch_async(self.isolationQueue, ^{
                            [connection.requests addObject:request];
                        });
                        self.currentRequest = request;
                    } else {
                        result = NO;
                    }
                } else {
                    result = NO;
                }
            } else {
                result = NO;
            }
        } else {
            result = NO;
        }

        if ( !result ) {
            [self.socket disconnect];
            return;
        }

        NSRange remainingDataRange = NSMakeRange(rangeOfFirstNewline.location + rangeOfFirstNewline.length, data.length - rangeOfFirstNewline.location - rangeOfFirstNewline.length);
        NSData* remainingData = [NSData dataWithBytesNoCopy:(void *)data.bytes + remainingDataRange.location length:remainingDataRange.length freeWhenDone:NO];
        if ( ! [self.currentRequest appendData:remainingData] ) {
            [self.socket disconnect];
            return;
        }

        // We've read the request headers
        if ( self.currentRequest.headersComplete ) {
            [self didReceiveCompleteRequestHeaders];
        } else {
            [self.socket disconnect];
            return;
        }

    } else if ( tag == CRHTTPConnectionSocketTagReadingRequestBody ) {

        // We are receiving data
        requestBodyReceivedBytesLength += data.length;
        [self didReceiveRequestBodyData:data];

        if (requestBodyReceivedBytesLength < requestBodyLength) {
            NSUInteger requestBodyLeftBytesLength = requestBodyLength - requestBodyReceivedBytesLength;
            NSUInteger bytesToRead = requestBodyLeftBytesLength < config.CRRequestBodyBufferSize ? requestBodyLeftBytesLength : config.CRRequestBodyBufferSize;
            [self.socket readDataToLength:bytesToRead withTimeout:config.CRHTTPConnectionReadBodyTimeout tag:CRHTTPConnectionSocketTagReadingRequestBody];
        } else {
            [self didReceiveCompleteRequest];
        }
    }
}

@end
