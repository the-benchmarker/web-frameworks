//
//  CRHTTPResponse.m
//  Criollo
//
//  Created by Cătălin Stan on 10/30/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRMessage_Internal.h"
#import "CRResponse_Internal.h"
#import "CRHTTPResponse.h"
#import "CRHTTPConnection.h"
#import "CRConnection_Internal.h"
#import "CRHTTPServer.h"
#import "CRHTTPServerConfiguration.h"
#import "GCDAsyncSocket.h"
#import "NSDate+RFC1123.h"

@interface CRHTTPResponse ()

- (nonnull NSMutableData *)initialResponseData;

@end

@implementation CRHTTPResponse

- (BOOL)isChunked {
    return [[self valueForHTTPHeaderField:@"Transfer-encoding"] isEqualToString:@"chunked"];
}

- (void)buildHeaders {
    if ( self.alreadyBuiltHeaders ) {
        return;
    }

    [self buildStatusLine];

    if ( [self valueForHTTPHeaderField:@"Date"] == nil ) {
        [self setValue:[NSDate date].rfc1123String forHTTPHeaderField:@"Date"];
    }

    if ( [self valueForHTTPHeaderField:@"Content-Type"] == nil ) {
        [self setValue:@"text/plain; charset=utf-8" forHTTPHeaderField:@"Content-Type"];
    }

    if ( [self valueForHTTPHeaderField:@"Connection"] == nil ) {
        NSString* connectionSpec = @"keep-alive";
        if ( self.version == CRHTTPVersion1_0 ) {
            connectionSpec = @"close";
        }
        [self setValue:connectionSpec forHTTPHeaderField:@"Connection"];
    }

    if ( [self valueForHTTPHeaderField:@"Content-length"] == nil ) {
        [self setValue:@"chunked" forHTTPHeaderField:@"Transfer-encoding"];
    }

    [super buildHeaders];

    self.alreadyBuiltHeaders = YES;
}

- (void)writeData:(NSData *)data finish:(BOOL)flag {
    if ( self.finished ) {
        @throw [NSException exceptionWithName:NSInternalInconsistencyException reason:@"Response is already finished" userInfo:nil];
    }

    NSMutableData* dataToSend = [self initialResponseData];

    if ( self.isChunked ) {
        // Chunk size + CRLF
        [dataToSend appendData: [[NSString stringWithFormat:@"%lx", (unsigned long)data.length] dataUsingEncoding:NSUTF8StringEncoding]];
        [dataToSend appendData: [CRConnection CRLFData]];
    }

    // The actual data
    [dataToSend appendData:data];
	
    if ( self.isChunked ) {
        // Chunk termination
        [dataToSend appendData: [CRConnection CRLFData]];
    }

    if ( flag && self.isChunked ) {
        [dataToSend appendData: [@"0" dataUsingEncoding:NSUTF8StringEncoding]];
        [dataToSend appendData:[CRConnection CRLFCRLFData]];
    }

    [super writeData:dataToSend finish:flag];
}

- (void)finish {
    [super finish];

    NSMutableData* dataToSend = [self initialResponseData];
    if ( self.isChunked ) {
        [dataToSend appendData: [@"0\r\n\r\n" dataUsingEncoding:NSUTF8StringEncoding]];
    } else {
        [dataToSend appendData: [@"\r\n" dataUsingEncoding:NSUTF8StringEncoding]];
    }

    [self.connection sendDataToSocket:dataToSend forRequest:self.request];
}

- (NSMutableData*)initialResponseData {
    NSMutableData* dataToSend = [NSMutableData dataWithCapacity:CRResponseDataInitialCapacity];

    if ( !self.alreadySentHeaders ) {
        [self buildHeaders];
        self.bodyData = nil;
        NSData* headersSerializedData = self.serializedData;
        NSData* headerData = [NSMutableData dataWithBytesNoCopy:(void*)headersSerializedData.bytes length:headersSerializedData.length freeWhenDone:NO];
        [dataToSend appendData:headerData];
        self.alreadySentHeaders = YES;
    }

    return dataToSend;
}


@end
