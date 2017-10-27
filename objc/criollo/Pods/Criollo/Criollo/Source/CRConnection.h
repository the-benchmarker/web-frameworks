//
//  CRConnection.h
//  Criollo
//
//  Created by Cătălin Stan on 10/23/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRTypes.h"

@class CRConnection, GCDAsyncSocket, CRServer, CRRequest, CRResponse;

NS_ASSUME_NONNULL_BEGIN
@protocol CRConnectionDelegate <NSObject>

- (void)connection:(CRConnection *)connection didReceiveRequest:(CRRequest *)request response:(CRResponse *)response;
- (void)connection:(CRConnection *)connection didFinishRequest:(CRRequest *)request response:(CRResponse *)response;

@end

@interface CRConnection : NSObject

@property (nonatomic, weak, nullable) id<CRConnectionDelegate> delegate;

@property (nonatomic, readonly) NSString* remoteAddress;
@property (nonatomic, readonly) NSUInteger remotePort;
@property (nonatomic, readonly) NSString* localAddress;
@property (nonatomic, readonly) NSUInteger localPort;

@end
NS_ASSUME_NONNULL_END
