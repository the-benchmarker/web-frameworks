//
//  CRStaticDirectoryManager.h
//  Criollo
//
//  Created by Cătălin Stan on 2/10/16.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRTypes.h"

@interface CRStaticDirectoryManager : NSObject

NS_ASSUME_NONNULL_BEGIN

@property (nonatomic, readonly, strong) NSString * directoryPath;
@property (nonatomic, readonly, copy) CRRouteBlock routeBlock;

@property (nonatomic, readonly) BOOL shouldCacheFiles;
@property (nonatomic, readonly) BOOL shouldGenerateDirectoryIndex;
@property (nonatomic, readonly) BOOL shouldShowHiddenFilesInDirectoryIndex;
@property (nonatomic, readonly) BOOL shouldFollowSymLinks;

+ (instancetype)managerWithDirectoryAtPath:(NSString *)directoryPath prefix:(NSString *)prefix;
+ (instancetype)managerWithDirectoryAtPath:(NSString *)directoryPath prefix:(NSString *)prefix options:(CRStaticDirectoryServingOptions)options;

- (instancetype)initWithDirectoryAtPath:(NSString *)directoryPath prefix:(NSString *)prefix;
- (instancetype)initWithDirectoryAtPath:(NSString *)directoryPath prefix:(NSString *)prefix options:(CRStaticDirectoryServingOptions)options NS_DESIGNATED_INITIALIZER;

NS_ASSUME_NONNULL_END

@end
