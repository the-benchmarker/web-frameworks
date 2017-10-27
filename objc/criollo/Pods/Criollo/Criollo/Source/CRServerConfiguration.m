//
//  CRServerConfiguration.m
//  Criollo
//
//  Created by Cătălin Stan on 10/25/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRServerConfiguration.h"

// Defaults
NSString* const CRServerDefaultInterface = @"";
NSUInteger const CRServerDefaultPort = 10781;
NSUInteger const CRConnectionDefaultReadTimeout = 5;
NSUInteger const CRConnectionDefaultWriteTimeout = 5;
NSUInteger const CRConnectionDefaultKeepAliveTimeout = 15;
NSUInteger const CRConnectionDefaultMaxKeepAliveConnections = 10;

// Keys
NSString* const CRServerInterfaceKey = @"CRServerInterface";
NSString* const CRServerPortKey = @"CRServerPort";
NSString* const CRConnectionReadTimeoutKey = @"CRConnectionReadTimeoutKey";
NSString* const CRConnectionWriteTimeoutKey = @"CRConnectionWriteTimeoutKey";
NSString* const CRConnectionKeepAliveTimeoutKey = @"CRConnectionKeepAliveTimeout";
NSString* const CRConnectionMaxKeepAliveConnectionsKey = @"CRConnectionMaxKeepAliveConnections";

@implementation CRServerConfiguration

- (instancetype) init {
    self = [super init];
    if ( self != nil ) {
        self.CRServerInterface = CRServerDefaultInterface;
        self.CRServerPort = CRServerDefaultPort;
        self.CRConnectionReadTimeout = CRConnectionDefaultReadTimeout;
        self.CRConnectionWriteTimeout = CRConnectionDefaultWriteTimeout;
        [self readConfiguration];
    }
    return self;
}

- (void)readConfiguration {

    NSBundle* mainBundle = [NSBundle mainBundle];
    NSUserDefaults *args = [NSUserDefaults standardUserDefaults];

    // Interface
    NSString* interface = [args stringForKey:@"i"];
    if ( interface.length == 0 ) {
        interface = [args stringForKey:@"interface"];
        if (interface.length == 0 && [mainBundle objectForInfoDictionaryKey:CRServerInterfaceKey] ) {
            interface = [mainBundle objectForInfoDictionaryKey:CRServerInterfaceKey];
        }
    }
    if (interface.length != 0) {
        self.CRServerInterface = interface;
    }

    // Port
    NSUInteger portNumber = [args integerForKey:@"p"];
    if ( portNumber == 0 ) {
        portNumber = [args integerForKey:@"port"];
        if ( portNumber == 0 && [mainBundle objectForInfoDictionaryKey:CRServerPortKey] ) {
            portNumber = [[mainBundle objectForInfoDictionaryKey:CRServerPortKey] integerValue];
        }
    }
    if ( portNumber != 0 ) {
        self.CRServerPort = portNumber;
    }

    // Timeouts
    if ( [mainBundle objectForInfoDictionaryKey:CRConnectionReadTimeoutKey] ) {
        self.CRConnectionReadTimeout = [[mainBundle objectForInfoDictionaryKey:CRConnectionReadTimeoutKey] integerValue];
    } else {
        self.CRConnectionReadTimeout = CRConnectionDefaultReadTimeout;
    }
    if ( [mainBundle objectForInfoDictionaryKey:CRConnectionWriteTimeoutKey] ) {
        self.CRConnectionWriteTimeout = [[mainBundle objectForInfoDictionaryKey:CRConnectionWriteTimeoutKey] integerValue];
    } else {
        self.CRConnectionWriteTimeout = CRConnectionDefaultWriteTimeout;
    }


    // Keep alive
    if ( [mainBundle objectForInfoDictionaryKey:CRConnectionKeepAliveTimeoutKey] ) {
        self.CRConnectionKeepAliveTimeout = [[mainBundle objectForInfoDictionaryKey:CRConnectionKeepAliveTimeoutKey] integerValue];
    } else {
        self.CRConnectionKeepAliveTimeout = CRConnectionDefaultKeepAliveTimeout;
    }
    if ( [mainBundle objectForInfoDictionaryKey:CRConnectionMaxKeepAliveConnectionsKey] ) {
        self.CRConnectionMaxKeepAliveConnections = [[mainBundle objectForInfoDictionaryKey:CRConnectionMaxKeepAliveConnectionsKey] integerValue];
    } else {
        self.CRConnectionMaxKeepAliveConnections = CRConnectionDefaultMaxKeepAliveConnections;
    }
}

@end
