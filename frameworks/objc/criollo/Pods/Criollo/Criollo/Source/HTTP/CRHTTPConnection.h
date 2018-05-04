//
//  CRHTTPConnection.h
//  Criollo
//
//  Created by Cătălin Stan on 10/25/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRConnection.h"

@class CRRequest, CRResponse;

#define CRHTTPConnectionSocketTagBeginReadingRequest                  10
#define CRHTTPConnectionSocketTagReadingRequestHeader                 11
#define CRHTTPConnectionSocketTagReadingRequestBody                   12

@interface CRHTTPConnection : CRConnection

@end
