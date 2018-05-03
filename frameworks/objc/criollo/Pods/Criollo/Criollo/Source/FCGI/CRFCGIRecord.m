//
//  CRFCGIRecord.m
//  Criollo
//
//  Created by Cătălin Stan on 10/30/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRFCGIRecord.h"

NSString* NSStringFromCRFCGIVersion(CRFCGIVersion version) {
    NSString* versionName;
    switch (version) {
        case CRFCGIVersion1:
            versionName = @"CRFCGIVersion1";
            break;
    }
    return versionName;
}

NSString* NSStringFromCRFCGIRecordType(CRFCGIRecordType recordType) {
    NSString* recordTypeName;
    switch (recordType) {
        case CRFCGIRecordTypeBeginRequest:
            recordTypeName = @"CRFCGIRecordTypeBeginRequest";
            break;
        case CRFCGIRecordTypeAbortRequest:
            recordTypeName = @"CRFCGIRecordTypeAbortRequest";
            break;
        case CRFCGIRecordTypeEndRequest:
            recordTypeName = @"CRFCGIRecordTypeEndRequest";
            break;
        case CRFCGIRecordTypeParams:
            recordTypeName = @"CRFCGIRecordTypeParams";
            break;
        case CRFCGIRecordTypeStdIn:
            recordTypeName = @"CRFCGIRecordTypeStdIn";
            break;
        case CRFCGIRecordTypeStdOut:
            recordTypeName = @"CRFCGIRecordTypeStdOut";
            break;
        case CRFCGIRecordTypeStdErr:
            recordTypeName = @"CRFCGIRecordTypeStdErr";
            break;
        case CRFCGIRecordTypeData:
            recordTypeName = @"CRFCGIRecordTypeData";
            break;
        case CRFCGIRecordTypeGetValues:
            recordTypeName = @"CRFCGIRecordTypeGetValues";
            break;
        case CRFCGIRecordTypeGetValuesResult:
            recordTypeName = @"CRFCGIRecordTypeGetValuesResult";
            break;
        case CRFCGIRecordTypeUnknown:
            recordTypeName = @"CRFCGIRecordTypeUnknown";
            break;
    }
    return recordTypeName;
}

NS_ASSUME_NONNULL_BEGIN
@interface CRFCGIRecord ()

@property (nonatomic, readonly, copy) NSData *headerProtocolData;

@end
NS_ASSUME_NONNULL_END

@implementation CRFCGIRecord

+ (instancetype)recordWithHeaderData:(NSData *)headerData {
    return [[CRFCGIRecord alloc] initWithHeaderData:headerData];
}

- (instancetype)init {
    return [self initWithHeaderData:[NSData data]];
}

- (instancetype)initWithHeaderData:(NSData *)data {
    self = [super init];
    if ( self != nil ) {
        if ( data != nil ) {
            const char *bytes = data.bytes;
            _version = bytes[0];
            _type = bytes[1];
            _requestID = (bytes[2] << 8) + bytes[3];
            _contentLength = (bytes[4] << 8) + bytes[5];
            _paddingLength = bytes[6];
            _reserved = bytes[7];
        }
    }
    return self;
}


@end
