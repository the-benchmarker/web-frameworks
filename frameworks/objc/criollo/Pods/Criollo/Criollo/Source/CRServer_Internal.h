//
//  CRServer_Internal.h
//  Criollo
//
//  Created by Cătălin Stan on 11/20/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRServer.h"

@class CRConnection;

NS_ASSUME_NONNULL_BEGIN
@interface CRServer ()

@property (nonatomic, strong) CRServerConfiguration * configuration;
@property (nonatomic, strong) NSMutableArray<CRConnection *> * connections;

- (void)didCloseConnection:(CRConnection *)connection;

@end
NS_ASSUME_NONNULL_END
