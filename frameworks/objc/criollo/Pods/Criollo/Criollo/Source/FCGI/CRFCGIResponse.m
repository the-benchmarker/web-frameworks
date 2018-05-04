//
//  CRFCGIResponse.m
//  Criollo
//
//  Created by Cătălin Stan on 10/30/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRResponse_Internal.h"
#import "CRFCGIResponse.h"
#import "CRApplication.h"
#import "CRFCGIServer.h"
#import "CRServer_Internal.h"
#import "CRFCGIServerConfiguration.h"
#import "CRFCGIConnection.h"
#import "CRConnection_Internal.h"
#import "CRMessage_Internal.h"
#import "CRFCGIRequest.h"
#import "CRFCGIRecord.h"
#import "GCDAsyncSocket.h"

NSString* NSStringFromCRFCGIProtocolStatus(CRFCGIProtocolStatus protocolStatus) {
    NSString* protocolStatusName;
    switch (protocolStatus) {
        case CRFCGIProtocolStatusRequestComplete:
            protocolStatusName = @"CRFCGIProtocolStatusRequestComplete";
            break;

        case CRFCGIProtocolStatusCannotMultiplexConnection:
            protocolStatusName = @"CRFCGIProtocolStatusCannotMultiplexConnection";
            break;

        case CRFCGIProtocolStatusOverloaded:
            protocolStatusName = @"CRFCGIProtocolStatusOverloaded";
            break;

        case CRFCGIProtocolStatusUnknownRole:
            protocolStatusName = @"CRFCGIProtocolStatusUnknownRole";
            break;
    }
    return protocolStatusName;
}

NS_ASSUME_NONNULL_BEGIN
@interface CRFCGIResponse () 

@property (nonatomic, readonly) NSData *endRequestRecordData;

- (NSData *)FCGIRecordDataWithContentData:(NSData * _Nullable)data;
- (NSMutableData *)initialResponseData;

@end
NS_ASSUME_NONNULL_END

@implementation CRFCGIResponse

- (instancetype)initWithConnection:(CRConnection *)connection HTTPStatusCode:(NSUInteger)HTTPStatusCode description:(NSString *)description version:(CRHTTPVersion)version {
    self = [super initWithConnection:connection HTTPStatusCode:HTTPStatusCode description:description version:version];
    if ( self != nil ) {
        self.protocolStatus = CRFCGIProtocolStatusRequestComplete;
        self.applicationStatus = 0;
    }
    return self;
}

- (void)buildHeaders {
    if ( self.alreadyBuiltHeaders ) {
        return;
    }

    [self buildStatusLine];

    if ( [self valueForHTTPHeaderField:@"Content-Type"] == nil ) {
        [self setValue:@"text/plain; charset=utf-8" forHTTPHeaderField:@"Content-Type"];
    }

    [super buildHeaders];

    self.alreadyBuiltHeaders = YES;
}

- (void)writeData:(NSData *)data finish:(BOOL)flag {

    if ( self.finished ) {
        @throw [NSException exceptionWithName:NSInternalInconsistencyException reason:@"Response is already finished" userInfo:nil];
    }

    NSMutableData* dataToSend = [self initialResponseData];

    // The actual data
    [dataToSend appendData:[self FCGIRecordDataWithContentData:data]];

    if ( flag ) {
        // End request record
        [dataToSend appendData:self.endRequestRecordData];
    }

    [super writeData:dataToSend finish:flag];
}

- (void)finish {
    [super finish];

    NSMutableData* dataToSend = [self initialResponseData];

    // End request record
    [dataToSend appendData:self.endRequestRecordData];

    [self.connection sendDataToSocket:dataToSend forRequest:self.request];
}

- (NSData*)FCGIRecordDataWithContentData:(NSData *)data {

    CRFCGIServerConfiguration* config = (CRFCGIServerConfiguration*)self.connection.server.configuration;

    NSMutableData* recordData = [NSMutableData dataWithCapacity:config.CRFCGIConnectionSocketWriteBuffer];
    NSUInteger offset = 0;

    do {
        NSUInteger chunkSize = data.length - offset > config.CRFCGIConnectionSocketWriteBuffer ? config.CRFCGIConnectionSocketWriteBuffer : data.length - offset;

        CRFCGIVersion version = CRFCGIVersion1;
        [recordData appendBytes:&version length:1];

        CRFCGIRecordType type = CRFCGIRecordTypeStdOut;
        [recordData appendBytes:&type length:1];

        UInt16 requestID = CFSwapInt16HostToBig(((CRFCGIRequest*)self.request).requestID);
        [recordData appendBytes:&requestID length:2];

        UInt16 contentLength = CFSwapInt16HostToBig(chunkSize);
        [recordData appendBytes:&contentLength length:2];

        UInt8 paddingLength = 0;
        [recordData appendBytes:&paddingLength length:1];

        UInt8 reserved = 0x00;
        [recordData appendBytes:&reserved length:1];

        NSData* chunk = [NSData dataWithBytesNoCopy:(char *)data.bytes + offset length:chunkSize freeWhenDone:NO];
        [recordData appendData:chunk];

        offset += chunkSize;
        
    } while (offset < data.length);

    return recordData;
    
}

- (NSData*)endRequestRecordData {

    NSMutableData* recordData = [NSMutableData data];

    CRFCGIVersion version = CRFCGIVersion1;
    [recordData appendBytes:&version length:1];

    CRFCGIRecordType type = CRFCGIRecordTypeEndRequest;
    [recordData appendBytes:&type length:1];

    UInt16 requestID = CFSwapInt16HostToBig(((CRFCGIRequest*)self.request).requestID);
    [recordData appendBytes:&requestID length:2];

    UInt16 contentLength = CFSwapInt16HostToBig(8);
    [recordData appendBytes:&contentLength length:2];

    UInt8 paddingLength = 0;
    [recordData appendBytes:&paddingLength length:1];

    UInt8 reserved = 0x00;
    [recordData appendBytes:&reserved length:1];

    CRFCGIApplicationStatus applicationStatus = CFSwapInt32HostToBig(self.applicationStatus);
    [recordData appendBytes:&applicationStatus length:4];

    CRFCGIProtocolStatus protocolStatus = self.protocolStatus;
    [recordData appendBytes:&protocolStatus length:1];

    // Pad the record to 8 bytes
    [recordData appendBytes:&reserved length:1];
    [recordData appendBytes:&reserved length:1];
    [recordData appendBytes:&reserved length:1];

    return recordData;
}

- (NSMutableData*)initialResponseData {
    NSMutableData* dataToSend = [NSMutableData dataWithCapacity:CRResponseDataInitialCapacity];

    if ( !self.alreadySentHeaders ) {
        [self buildHeaders];
        self.bodyData = nil;
        NSData* headersSerializedData = self.serializedData;
        NSData* headerData = [self FCGIRecordDataWithContentData:[NSData dataWithBytesNoCopy:(void*)headersSerializedData.bytes length:headersSerializedData.length freeWhenDone:NO]];
        [dataToSend appendData:headerData];
        self.alreadySentHeaders = YES;
    }

    return dataToSend;

}


@end
