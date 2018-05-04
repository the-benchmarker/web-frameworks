//
//  CRMessage.m
//  Criollo
//
//  Created by Cătălin Stan on 29/04/15.
//
//

#import "CRMessage.h"
#import "CRMessage_Internal.h"

NSString * NSStringFromCRHTTPVersion(CRHTTPVersion version) {
    switch (version) {
        case CRHTTPVersion1_0:
            return (__bridge NSString*)kCFHTTPVersion1_0;
        case CRHTTPVersion1_1:
            return (__bridge NSString*)kCFHTTPVersion1_1;
//        case CRHTTPVersion2_0:
//            return (__bridge NSString*)kCFHTTPVersion2_0;
    }
}

CRHTTPVersion CRHTTPVersionMake(NSString * versionSpec) {
    CRHTTPVersion version = CRHTTPVersion1_1;
    if ( [versionSpec isEqualToString:(__bridge NSString*)kCFHTTPVersion1_1] ) {
        version = CRHTTPVersion1_1;
//    } else if ( [versionSpec isEqualToString:(__bridge NSString*)kCFHTTPVersion2_0] ) {    }
//        version = CRHTTPVersion2_0;
    } else if ( [versionSpec isEqualToString:(__bridge NSString*)kCFHTTPVersion1_0] ) {
        version = CRHTTPVersion1_0;
    }
    return version;
}


NSString * const CRHTTPMethodNoneValue = @"NONE";
NSString * const CRHTTPMethodGetValue = @"GET";
NSString * const CRHTTPMethodPostValue = @"POST";
NSString * const CRHTTPMethodPutValue = @"PUT";
NSString * const CRHTTPMethodDeleteValue = @"DELETE";
NSString * const CRHTTPMethodPatchValue = @"PATCH";
NSString * const CRHTTPMethodOptionsValue = @"OPTIONS";
NSString * const CRHTTPMethodHeadValue = @"HEAD";
NSString * const CRHTTPMethodAllValue = @"ALL";

NSString * NSStringFromCRHTTPMethod(CRHTTPMethod method) {
    switch (method) {
        case CRHTTPMethodGet:
            return CRHTTPMethodGetValue;
        case CRHTTPMethodPost:
            return CRHTTPMethodPostValue;
        case CRHTTPMethodPut:
            return CRHTTPMethodPutValue;
        case CRHTTPMethodDelete:
            return CRHTTPMethodDeleteValue;
        case CRHTTPMethodPatch:
            return CRHTTPMethodPatchValue;
        case CRHTTPMethodOptions:
            return CRHTTPMethodOptionsValue;
        case CRHTTPMethodHead:
            return CRHTTPMethodHeadValue;
        case CRHTTPMethodAll:
            return CRHTTPMethodAllValue;
        case CRHTTPMethodNone:
            return CRHTTPMethodNoneValue;
    }
}

CRHTTPMethod CRHTTPMethodMake(NSString * methodSpec) {
    CRHTTPMethod HTTPMethod;
    if ( [methodSpec isEqualToString:CRHTTPMethodGetValue] ) {
        HTTPMethod = CRHTTPMethodGet;
    } else if ( [methodSpec isEqualToString:CRHTTPMethodPostValue] ) {
        HTTPMethod = CRHTTPMethodPost;
    } else if ( [methodSpec isEqualToString:CRHTTPMethodPutValue] ) {
        HTTPMethod = CRHTTPMethodPut;
    } else if ( [methodSpec isEqualToString:CRHTTPMethodDeleteValue] ) {
        HTTPMethod = CRHTTPMethodDelete;
    } else if ( [methodSpec isEqualToString:CRHTTPMethodPatchValue] ) {
        HTTPMethod = CRHTTPMethodPatch;
    } else if ( [methodSpec isEqualToString:CRHTTPMethodOptionsValue] ) {
        HTTPMethod = CRHTTPMethodOptions;
    } else if ( [methodSpec isEqualToString:CRHTTPMethodHeadValue] ) {
        HTTPMethod = CRHTTPMethodHead;
    } else if ( [methodSpec isEqualToString:CRHTTPMethodAllValue] ) {
        HTTPMethod = CRHTTPMethodAll;
    } else {
        HTTPMethod = CRHTTPMethodNone;
    }
    return HTTPMethod;
}

@implementation CRMessage

static const NSArray<NSString *> *acceptedHTTPMethods;

+ (void)initialize {
    acceptedHTTPMethods = @[CRHTTPMethodGetValue, CRHTTPMethodPostValue, CRHTTPMethodPutValue, CRHTTPMethodDeleteValue, CRHTTPMethodPatchValue, CRHTTPMethodOptionsValue, CRHTTPMethodHeadValue];

}

+ (NSArray<NSString *> *)acceptedHTTPMethods {
    return (NSArray<NSString *> *)acceptedHTTPMethods;
}

- (instancetype)init {
    self = [super init];
    if ( self != nil ) {
        self.message = CFBridgingRelease( CFHTTPMessageCreateEmpty(NULL, YES) );
    }
    return self;
}

- (CRHTTPVersion)version {
    return CRHTTPVersionMake((__bridge_transfer NSString *)CFHTTPMessageCopyVersion((__bridge CFHTTPMessageRef _Nonnull)(self.message)));
}

- (NSData *)serializedData {
    return (__bridge_transfer NSData *)CFHTTPMessageCopySerializedMessage((__bridge CFHTTPMessageRef _Nonnull)(self.message));
}

- (NSDictionary<NSString*, NSString*> *)allHTTPHeaderFields {
    return (__bridge_transfer NSDictionary *)CFHTTPMessageCopyAllHeaderFields((__bridge CFHTTPMessageRef _Nonnull)(self.message));
}

- (NSString *)valueForHTTPHeaderField:(NSString *)HTTPHeaderField {
    return (__bridge_transfer NSString *)CFHTTPMessageCopyHeaderFieldValue((__bridge CFHTTPMessageRef _Nonnull)(self.message), (__bridge CFStringRef)HTTPHeaderField);
}

- (NSData *)bodyData {
    return (__bridge_transfer NSData *)CFHTTPMessageCopyBody((__bridge CFHTTPMessageRef _Nonnull)(self.message));
}

- (void)setBodyData:(NSData *)bodyData {
    CFHTTPMessageSetBody((__bridge CFHTTPMessageRef _Nonnull)(self.message), (__bridge CFDataRef)bodyData);
}

- (BOOL)headersComplete {
    return CFHTTPMessageIsHeaderComplete((__bridge CFHTTPMessageRef _Nonnull)(self.message));
}

@end
