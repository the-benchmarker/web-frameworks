//
//  CRRouteMatchingResult.m
//  Criollo
//
//  Created by Cătălin Stan on 24/07/16.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRRouteMatchingResult.h"
#import "CRRouteMatchingResult_Internal.h"
#import "CRRoute.h"

@implementation CRRouteMatchingResult

- (instancetype)init {
    return [self initWithRoute:[CRRoute new] matches:nil];
}

- (instancetype)initWithRoute:(CRRoute *)route matches:(NSArray<NSString *> *)matches {
    self = [super init];
    if ( self != nil ) {
        _route = route;
        _matches = matches;
    }
    return self;
}

+ (instancetype)routeMatchingResultWithRoute:(CRRoute *)route matches:(NSArray<NSString *> *)matches {
    return [[CRRouteMatchingResult alloc] initWithRoute:route matches:matches];
}

@end
