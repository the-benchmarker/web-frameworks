//
//  CRHTTPServerConfiguration.m
//  Criollo
//
//  Created by Cătălin Stan on 10/30/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRHTTPServerConfiguration.h"

// Defaults
NSUInteger const CRHTTPConnectionDefaultReadHeaderTimeout = 2;
NSUInteger const CRHTTPConnectionDefaultReadBodyTimeout = 2;
NSUInteger const CRRequestDefaultMaxHeaderLength = 20 * 1024;
NSUInteger const CRRequestDefaultBodyBufferSize = 8 * 1024 * 1024;

// Keys
NSString* const CRHTTPConnectionReadHeaderTimeoutKey = @"CRHTTPConnectionReadHeaderTimeout";
NSString* const CRHTTPConnectionReadBodyTimeoutKey = @"CRHTTPConnectionReadBodyTimeout";
NSString* const CRRequestMaxHeaderLengthKey = @"CRRequestMaxHeaderLength";
NSString* const CRRequestBodyBufferSizeKey = @"CRRequestBodyBufferSize";

@implementation CRHTTPServerConfiguration

- (void)readConfiguration {
    [super readConfiguration];

    NSBundle* mainBundle = [NSBundle mainBundle];

    if ( [mainBundle objectForInfoDictionaryKey:CRHTTPConnectionReadHeaderTimeoutKey] ) {
        self.CRHTTPConnectionReadHeaderTimeout = [[mainBundle objectForInfoDictionaryKey:CRHTTPConnectionReadHeaderTimeoutKey] integerValue];
    } else {
        self.CRHTTPConnectionReadHeaderTimeout = CRHTTPConnectionDefaultReadHeaderTimeout;
    }
    if ( [mainBundle objectForInfoDictionaryKey:CRHTTPConnectionReadBodyTimeoutKey] ) {
        self.CRHTTPConnectionReadBodyTimeout = [[mainBundle objectForInfoDictionaryKey:CRHTTPConnectionReadBodyTimeoutKey] integerValue];
    } else {
        self.CRHTTPConnectionReadBodyTimeout = CRHTTPConnectionDefaultReadBodyTimeout;
    }

    // Limits
    if ( [mainBundle objectForInfoDictionaryKey:CRRequestMaxHeaderLengthKey] ) {
        self.CRRequestMaxHeaderLength = [[mainBundle objectForInfoDictionaryKey:CRRequestMaxHeaderLengthKey] integerValue];
    } else {
        self.CRRequestMaxHeaderLength = CRRequestDefaultMaxHeaderLength;
    }

    // Buffers
    if ( [mainBundle objectForInfoDictionaryKey:CRRequestBodyBufferSizeKey] ) {
        self.CRRequestBodyBufferSize = [[mainBundle objectForInfoDictionaryKey:CRRequestBodyBufferSizeKey] integerValue];
    } else {
        self.CRRequestBodyBufferSize = CRRequestDefaultBodyBufferSize;
    }

}


@end
