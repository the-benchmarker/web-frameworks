//
//  CRFCGIServer.m
//  Criollo
//
//  Created by Cătălin Stan on 10/25/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRFCGIServer.h"
#import "CRServer_Internal.h"
#import "CRFCGIConnection.h"
#import "CRConnection_Internal.h"
#import "CRFCGIServerConfiguration.h"

@implementation CRFCGIServer

- (instancetype)initWithDelegate:(id<CRServerDelegate>)delegate delegateQueue:(dispatch_queue_t)delegateQueue {
    self = [super initWithDelegate:delegate delegateQueue:delegateQueue];
    if ( self != nil ) {
        self.configuration = [[CRFCGIServerConfiguration alloc] init];
    }
    return self;
}

- (CRConnection*)newConnectionWithSocket:(GCDAsyncSocket*)socket {
    CRFCGIConnection* connection = [[CRFCGIConnection alloc] initWithSocket:socket server:self];
    return connection;
}

@end
