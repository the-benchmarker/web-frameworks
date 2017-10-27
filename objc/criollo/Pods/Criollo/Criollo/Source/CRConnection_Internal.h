//
//  CRConnection_Internal.h
//  Criollo
//
//  Created by Cătălin Stan on 11/20/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRConnection.h"

@class GCDAsyncSocket, CRServer, CRRequest;

NS_ASSUME_NONNULL_BEGIN

@interface CRConnection ()

@property (nonatomic, strong, nullable) GCDAsyncSocket* socket;
@property (nonatomic, weak) CRServer* server;

@property (nonatomic, strong, nullable) NSMutableArray<CRRequest *> * requests;
@property (nonatomic, weak, nullable) CRRequest* currentRequest;

@property (nonatomic, readonly, strong, nullable) dispatch_queue_t isolationQueue;

@property (nonatomic, readonly) BOOL willDisconnect;

+ (NSData *)CRLFData;
+ (NSData *)CRLFCRLFData;

- (instancetype)initWithSocket:(GCDAsyncSocket * _Nullable)socket server:(CRServer * _Nullable)server NS_DESIGNATED_INITIALIZER;

- (CRResponse *)responseWithHTTPStatusCode:(NSUInteger)HTTPStatusCode;
- (CRResponse *)responseWithHTTPStatusCode:(NSUInteger)HTTPStatusCode description:(NSString * _Nullable)description;
- (CRResponse *)responseWithHTTPStatusCode:(NSUInteger)HTTPStatusCode description:(NSString * _Nullable)description version:(CRHTTPVersion)version;

- (void)startReading;
- (void)didReceiveCompleteRequestHeaders;
- (void)didReceiveRequestBodyData:(NSData *)data;
- (void)didReceiveCompleteRequest;

- (void)sendDataToSocket:(NSData *)data forRequest:(CRRequest *)request;
- (void)didFinishResponseForRequest:(CRRequest *)request;

@end

NS_ASSUME_NONNULL_END
