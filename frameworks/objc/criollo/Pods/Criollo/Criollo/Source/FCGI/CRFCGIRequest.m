//
//  CRFCGIRequest.m
//  Criollo
//
//  Created by Cătălin Stan on 10/31/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRFCGIRequest.h"

NSString* NSStringFromCRFCGIRequestRole(CRFCGIRequestRole requestRole) {
    NSString* requestRoleName;
    switch(requestRole) {
        case CRFCGIRequestRoleResponder:
            requestRoleName = @"CRFCGIRequestRoleResponder";
            break;
        case CRFCGIRequestRoleAuthorizer:
            requestRoleName = @"CRFCGIRequestRoleAuthorizer";
            break;
        case CRFCGIRequestRoleFilter:
            requestRoleName = @"CRFCGIRequestRoleFilter";
            break;
    }
    return requestRoleName;
}

@implementation CRFCGIRequest

- (BOOL)shouldCloseConnection {
    return (self.requestFlags & CRFCGIRequestFlagKeepAlive) == 0;
}

@end
