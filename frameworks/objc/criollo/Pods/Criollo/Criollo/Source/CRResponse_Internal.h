//
//  CRResponse_Internal.h
//  Criollo
//
//  Created by Cătălin Stan on 11/20/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRResponse.h"

NS_ASSUME_NONNULL_BEGIN
@interface CRResponse ()

@property (nonatomic, assign) NSUInteger proposedStatusCode;
@property (nonatomic, strong, nullable) NSString* proposedStatusDescription;

@property (nonatomic, assign) BOOL alreadySentHeaders;
@property (nonatomic, assign) BOOL alreadyBuiltHeaders;
@property (nonatomic, readonly) BOOL finished;
@property (nonatomic, readonly) BOOL hasWrittenBodyData;

- (instancetype)initWithConnection:(CRConnection *)connection HTTPStatusCode:(NSUInteger)HTTPStatusCode;
- (instancetype)initWithConnection:(CRConnection *)connection HTTPStatusCode:(NSUInteger)HTTPStatusCode description:(NSString * _Nullable)description;
- (instancetype)initWithConnection:(CRConnection *)connection HTTPStatusCode:(NSUInteger)HTTPStatusCode description:(NSString * _Nullable)description version:(CRHTTPVersion)version NS_DESIGNATED_INITIALIZER;

- (void)writeData:(NSData *)data finish:(BOOL)flag;

- (void)buildStatusLine;
- (void)buildHeaders;

@end
NS_ASSUME_NONNULL_END
