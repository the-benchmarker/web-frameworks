//
//  CRFCGIConnection.m
//  Criollo
//
//  Created by Cătălin Stan on 10/25/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRResponse_Internal.h"
#import "CRFCGIResponse.h"
#import "CRFCGIConnection.h"
#import "CRConnection_Internal.h"
#import "CRApplication.h"
#import "CRServer_Internal.h"
#import "CRFCGIServer.h"
#import "CRFCGIServerConfiguration.h"
#import "CRFCGIRequest.h"
#import "CRRequest_Internal.h"
#import "CRFCGIRecord.h"
#import "GCDAsyncSocket.h"

NS_ASSUME_NONNULL_BEGIN

@interface CRFCGIConnection () {
    NSUInteger currentRequestBodyLength;
    NSUInteger currentRequestBodyReceivedBytesLength;
    
    BOOL didPerformInitialRead;

    CRFCGIRecord* currentRecord;
    UInt16 currentRequestID;
    CRFCGIRequestRole currentRequestRole;
    CRFCGIRequestFlags currentRequestFlags;
    NSMutableDictionary* currentRequestParams;
}

- (void)appendParamsFromData:(NSData *)paramsData length:(NSUInteger)dataLength;

@end

NS_ASSUME_NONNULL_END

@implementation CRFCGIConnection

#pragma mark - Data

- (void)startReading {
    [super startReading];

    currentRequestBodyLength = 0;
    currentRequestBodyReceivedBytesLength = 0;

    currentRecord = nil;
    currentRequestID = 0;
    currentRequestRole = CRFCGIRequestRoleResponder;
    currentRequestFlags = 0;
    currentRequestParams = [NSMutableDictionary dictionary];

    CRFCGIServerConfiguration* config = (CRFCGIServerConfiguration*)self.server.configuration;

    // Read the begin request record
    NSUInteger timeout = (didPerformInitialRead ? config.CRConnectionKeepAliveTimeout : config.CRConnectionReadTimeout) + config.CRFCGIConnectionReadRecordTimeout;
    [self.socket readDataToLength:CRFCGIRecordHeaderLength withTimeout:timeout tag:CRFCGIConnectionSocketTagReadRecordHeader];
}

- (void)didReceiveCompleteRequestHeaders {

    // Create HTTP headers from FCGI Params
    NSMutableData* headersData = [NSMutableData data];
    [self.currentRequest.env enumerateKeysAndObjectsUsingBlock:^(NSString * _Nonnull key, NSString * _Nonnull obj, BOOL * _Nonnull stop) {
        @autoreleasepool {
            if ( ![key hasPrefix:@"HTTP_"] ) {
                return;
            }
            NSArray<NSString*>* headerParts = [[key substringFromIndex:5] componentsSeparatedByString:@"_"];
            NSMutableArray<NSString*>* transformedHeaderParts = [NSMutableArray arrayWithCapacity:headerParts.count];

            [headerParts enumerateObjectsUsingBlock:^(NSString * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                @autoreleasepool {
                    NSString* transformedHeaderPart = [[obj substringToIndex:1].uppercaseString stringByAppendingString:[obj substringFromIndex:1].lowercaseString];
                    [transformedHeaderParts addObject:transformedHeaderPart];
                }
            }];

            NSString* headerName = [transformedHeaderParts componentsJoinedByString:@"-"];

            NSData* headerData = [[NSString stringWithFormat:@"%@: %@", headerName, obj] dataUsingEncoding:NSUTF8StringEncoding];
            [headersData appendData:headerData];
            [headersData appendData:[CRConnection CRLFData]];
        }
    }];

    [self.currentRequest appendData:headersData];
    [self.currentRequest appendData:[CRConnection CRLFData]];

    [super didReceiveCompleteRequestHeaders];

    currentRequestBodyLength = [self.currentRequest.env[@"CONTENT_LENGTH"] integerValue];
    CRFCGIServerConfiguration* config = (CRFCGIServerConfiguration*)self.server.configuration;
    [self.socket readDataToLength:CRFCGIRecordHeaderLength withTimeout:config.CRFCGIConnectionReadRecordTimeout tag:CRFCGIConnectionSocketTagReadRecordHeader];
}

#pragma mark - Responses

- (CRResponse *)responseWithHTTPStatusCode:(NSUInteger)HTTPStatusCode description:(NSString *)description version:(CRHTTPVersion)version {
    return [[CRFCGIResponse alloc] initWithConnection:self HTTPStatusCode:HTTPStatusCode description:description version:version];
}

#pragma mark - Record Processing

- (void)appendParamsFromData:(NSData*)paramsData length:(NSUInteger)dataLength {

    void(^parseValuePairBlock)(NSUInteger*, NSString**, NSString**, NSUInteger*) = ^(NSUInteger* offset, NSString** name, NSString** value, NSUInteger* bytesRead) {

        @autoreleasepool {
            // Refer to http://www.fastcgi.com/drupal/node/6?q=node/22#S3.4 for rules in parsing dictionaries

            NSUInteger initialOffset = *offset;

            UInt8 pos0, pos1, pos4;
            UInt8 valueLengthB3, valueLengthB2, valueLengthB1, valueLengthB0;
            UInt8 nameLengthB3, nameLengthB2, nameLengthB1, nameLengthB0;
            UInt32 nameLength, valueLength;

            const char *bytes = paramsData.bytes;

            pos0 = bytes[*offset + 0];
            pos1 = bytes[*offset + 1];
            pos4 = bytes[*offset + 4];

            if (pos0 >> 7 == 0) {

                // NameValuePair11 or 14
                nameLength = pos0;

                if (pos1 >> 7 == 0) {
                    // NameValuePair11
                    valueLength = pos1;
                    *offset += 2;
                } else {
                    //NameValuePair14
                    valueLengthB3 = bytes[*offset + 1];
                    valueLengthB2 = bytes[*offset + 2];
                    valueLengthB1 = bytes[*offset + 3];
                    valueLengthB0 = bytes[*offset + 4];
                    valueLength = ((valueLengthB3 & 0x7f) << 24) + (valueLengthB2 << 16) + (valueLengthB1 << 8) + valueLengthB0;
                    *offset += 5;
                }

            } else {

                // NameValuePair41 or 44
                nameLengthB3 = bytes[*offset + 1];
                nameLengthB2 = bytes[*offset + 2];
                nameLengthB1 = bytes[*offset + 3];
                nameLengthB0 = bytes[*offset + 4];
                nameLength = ((nameLengthB3 & 0x7f) << 24) + (nameLengthB2 << 16) + (nameLengthB1 << 8) + nameLengthB0;

                if (pos4 >> 7 == 0) {
                    //NameValuePair41
                    valueLength = pos4;
                    *offset += 5;
                } else {
                    //NameValuePair44
                    valueLengthB3 = bytes[*offset + 4];
                    valueLengthB2 = bytes[*offset + 5];
                    valueLengthB1 = bytes[*offset + 6];
                    valueLengthB0 = bytes[*offset + 7];
                    valueLength = ((valueLengthB3 & 0x7f) << 24) + (valueLengthB2 << 16) + (valueLengthB1 << 8) + valueLengthB0;
                    *offset += 8;
                }
            }

            *name = [[NSString alloc] initWithBytesNoCopy:(void *)paramsData.bytes + *offset length:nameLength encoding:NSUTF8StringEncoding freeWhenDone:NO];
            *offset += nameLength;
            
            *value = [[NSString alloc] initWithBytes:(void *)paramsData.bytes + *offset length:valueLength encoding:NSUTF8StringEncoding];
            *offset += valueLength;
            
            if ( bytesRead != NULL ) {
                *bytesRead = *offset - initialOffset;
            }
        }
    };

    NSUInteger startOffset = 0;
    NSString *paramName, *paramValue;

    while ( startOffset < dataLength ) {
        parseValuePairBlock(&startOffset, &paramName, &paramValue, NULL);
        if ( paramName.length != 0 && paramValue != nil ) {
            currentRequestParams[paramName] = paramValue;
        }
    }

}

#pragma mark - GCDAsyncSocketDelegate

- (void)socket:(GCDAsyncSocket *)sock didReadData:(NSData*)data withTag:(long)tag {

    didPerformInitialRead = YES;

    CRFCGIServerConfiguration* config = (CRFCGIServerConfiguration*)self.server.configuration;

    switch (tag) {

        case CRFCGIConnectionSocketTagReadRecordHeader:
            currentRecord = [[CRFCGIRecord alloc] initWithHeaderData:data];

//            NSLog(@" * Header: %@ %hu", NSStringFromCRFCGIRecordType(currentRecord.type), currentRecord.contentLength);

            // Process the record header
            if (currentRecord.contentLength == 0) {

                // Zero-length content records are markers
                switch (currentRecord.type) {
                    case CRFCGIRecordTypeParams: {
                        // We've finished reading the parameters
                        NSString* methodSpec = currentRequestParams[@"REQUEST_METHOD"];
                        NSString* path = currentRequestParams[@"DOCUMENT_URI"];
                        NSString* versionSpec = currentRequestParams[@"SERVER_PROTOCOL"];
                        NSString* host = currentRequestParams[@"HTTP_HOST"];

                        NSURL* URL = [NSURL URLWithString:[NSString stringWithFormat:@"http://%@%@", host, path]];
                        CRFCGIRequest* request = [[CRFCGIRequest alloc] initWithMethod:CRHTTPMethodMake(methodSpec) URL:URL version:CRHTTPVersionMake(versionSpec) connection:self env:currentRequestParams];
                        CRFCGIConnection * __weak connection = self;
                        dispatch_async(self.isolationQueue, ^{
                            [connection.requests addObject:request];
                        });
                        request.requestID = currentRequestID;
                        request.requestRole = currentRequestRole;
                        request.requestFlags = currentRequestFlags;
                        self.currentRequest = request;

                        [self didReceiveCompleteRequestHeaders];
                    }
                        break;

                    case CRFCGIRecordTypeStdIn: {

                        if ( currentRequestBodyLength == currentRequestBodyReceivedBytesLength ) {
                            [self didReceiveCompleteRequest];
                        } else {
                            [self.socket disconnectAfterWriting];
                        }
                    }
                        break;


                    default:
                        break;
                }

            } else {

                // Read the data content of the record
                [self.socket readDataToLength:(currentRecord.contentLength + currentRecord.paddingLength) withTimeout:config.CRFCGIConnectionReadRecordTimeout tag:CRFCGIConnectionSocketTagReadRecordContent];

            }
            break;

        case CRFCGIConnectionSocketTagReadRecordContent:

//            NSLog(@" * Content: %@ %lu", NSStringFromCRFCGIRecordType(currentRecord.type), data.length);

            // Process the header content data
            switch (currentRecord.type) {
                case CRFCGIRecordTypeBeginRequest:
                    // Request ID
                    currentRequestID = currentRecord.requestID;

                    // Request role
                    [data getBytes:&currentRequestRole range:NSMakeRange(0, 2)];
                    currentRequestRole = CFSwapInt16BigToHost(currentRequestRole);

                    // Request flags
                    [data getBytes:&currentRequestFlags range:NSMakeRange(2, 1)];

                    // Go on reaading the next record
                    [self.socket readDataToLength:CRFCGIRecordHeaderLength withTimeout:config.CRFCGIConnectionReadRecordTimeout tag:CRFCGIConnectionSocketTagReadRecordHeader];

                    break;

                case CRFCGIRecordTypeParams:
                    // We are receiving the params
                    [self appendParamsFromData:data length:(currentRecord.contentLength - currentRecord.paddingLength)];

                    // Go on reaading the next record
                    [self.socket readDataToLength:CRFCGIRecordHeaderLength withTimeout:config.CRFCGIConnectionReadRecordTimeout tag:CRFCGIConnectionSocketTagReadRecordHeader];
                    break;

                case CRFCGIRecordTypeStdIn: {

                    NSData* currentRecordContentData = [NSData dataWithBytesNoCopy:(void *)data.bytes length:currentRecord.contentLength freeWhenDone:NO];
                    [self didReceiveRequestBodyData:currentRecordContentData];

                    currentRequestBodyReceivedBytesLength += currentRecord.contentLength;

                    // Go on reaading the next record
                    [self.socket readDataToLength:CRFCGIRecordHeaderLength withTimeout:config.CRFCGIConnectionReadRecordTimeout tag:CRFCGIConnectionSocketTagReadRecordHeader];
                }
                    break;

                default:
                    break;
            }

            break;
            
        default:
            break;
    }
    
}

@end
