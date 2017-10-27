//
//  CRFCGIRequest.h
//  Criollo
//
//  Created by Cătălin Stan on 10/31/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRRequest.h"

#define CRFCGIRequestFlagKeepAlive   1

typedef NS_ENUM(UInt8, CRFCGIRequestRole) {
    CRFCGIRequestRoleResponder = 1,
    CRFCGIRequestRoleAuthorizer = 2,
    CRFCGIRequestRoleFilter = 3
};

typedef UInt8  CRFCGIRequestFlags;

NS_ASSUME_NONNULL_BEGIN

FOUNDATION_EXPORT  NSString * NSStringFromCRFCGIRequestRole(CRFCGIRequestRole requestRole);

@interface CRFCGIRequest : CRRequest

@property (nonatomic, assign) UInt16 requestID;
@property (nonatomic, assign) CRFCGIRequestRole requestRole;
@property (nonatomic, assign) CRFCGIRequestFlags requestFlags;

@end
NS_ASSUME_NONNULL_END
