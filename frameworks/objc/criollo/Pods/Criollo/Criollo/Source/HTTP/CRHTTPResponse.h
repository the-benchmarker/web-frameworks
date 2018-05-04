//
//  CRHTTPResponse.h
//  Criollo
//
//  Created by Cătălin Stan on 10/30/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRResponse.h"

@interface CRHTTPResponse : CRResponse

@property (nonatomic, readonly) BOOL isChunked;

@end
