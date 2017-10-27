//
//  AppDelegate.m
//  server_objc_criollo
//
//  Created by Cătălin Stan on 25/10/2017.
//  Copyright © 2017 Cătălin Stan. All rights reserved.
//

#import "AppDelegate.h"

@interface AppDelegate () <CRServerDelegate>

@property (nonatomic, strong) CRHTTPServer *server;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
    self.server = [[CRHTTPServer alloc] initWithDelegate:self];
    
    // GET '/' return status code 200 with empty body
    [self.server add:@"/" block:^(CRRequest *request, CRResponse * response, CRRouteCompletionBlock  completionHandler) {
        [response finish];
    }];
    
    // GET '/user/:id' return status code 200 with the id
    [self.server add:@"/user/:id" block:^(CRRequest *request, CRResponse * response, CRRouteCompletionBlock  completionHandler) {
        [response send:request.query[@"id"] ?: @""];
    }];
    
    // POST '/user' return status code 200 with empty body
    [self.server post:@"/user" block:^(CRRequest *request, CRResponse * response, CRRouteCompletionBlock  completionHandler) {
        [response finish];
    }];
    
    NSError *error;
    if ( ! [self.server startListening:&error portNumber:3000] ) {
        [CRApp logErrorFormat:@"Error starting server: %@", error];
        [CRApp terminate:nil];
        return;
    }
    
    [CRApp logFormat:@"%@ Successfully started server", NSDate.date];
}

- (void)applicationWillTerminate:(NSNotification *)aNotification {
    [CRApp logFormat:@"%@ Sutting down server.", NSDate.date];
    [self.server stopListening];
}

- (CRApplicationTerminateReply)applicationShouldTerminate:(CRApplication *)sender {
    static CRApplicationTerminateReply reply;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        reply = CRTerminateLater;
        [CRApp logFormat:@"%@ Closing server connections.", NSDate.date];
        [self.server closeAllConnections:^{
            [CRApp replyToApplicationShouldTerminate:YES];
        }];
    });
    return reply;
}



@end
