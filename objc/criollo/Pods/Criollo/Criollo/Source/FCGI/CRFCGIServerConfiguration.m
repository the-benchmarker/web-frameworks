//
//  CRFCGIServerConfiguration.m
//  Criollo
//
//  Created by Cătălin Stan on 10/30/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRFCGIServerConfiguration.h"

// Defaults
NSUInteger const CRFCGIConnectionDefaultReadRecordTimeout = 5;
NSUInteger const CRFCGIConnectionDefaultSocketWriteBuffer = 32 * 1024;

// Keys
NSString* const CRFCGIConnectionReadRecordTimeoutKey = @"CRFCGIConnectionReadRecordTimeout";
NSString* const CRFCGIConnectionSocketWriteBufferKey = @"CRFCGIConnectionSocketWriteBuffer";

@implementation CRFCGIServerConfiguration

- (void)readConfiguration {

    [super readConfiguration];

    NSBundle* mainBundle = [NSBundle mainBundle];

    if ( [mainBundle objectForInfoDictionaryKey:CRFCGIConnectionReadRecordTimeoutKey] ) {
        self.CRFCGIConnectionReadRecordTimeout = [[mainBundle objectForInfoDictionaryKey:CRFCGIConnectionReadRecordTimeoutKey] integerValue];
    } else {
        self.CRFCGIConnectionReadRecordTimeout = CRFCGIConnectionDefaultReadRecordTimeout;
    }

    if ( [mainBundle objectForInfoDictionaryKey:CRFCGIConnectionSocketWriteBufferKey] ) {
        self.CRFCGIConnectionSocketWriteBuffer = [[mainBundle objectForInfoDictionaryKey:CRFCGIConnectionSocketWriteBufferKey] integerValue];
    } else {
        self.CRFCGIConnectionSocketWriteBuffer = CRFCGIConnectionDefaultSocketWriteBuffer;
    }

}


@end
