//
//  CRRoute.h
//  Criollo
//
//  Created by Cătălin Stan on 11/8/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRTypes.h"

@class CRRequest, CRResponse;

NS_ASSUME_NONNULL_BEGIN

@interface CRRoute : NSObject

@property (nonatomic) CRHTTPMethod method;
@property (nonatomic, strong) NSString * path;
@property (nonatomic) BOOL recursive;
@property (nonatomic, copy) CRRouteBlock block;

- (instancetype)initWithBlock:(CRRouteBlock)block method:(CRHTTPMethod)method path:(NSString * _Nullable)path recursive:(BOOL)recursive NS_DESIGNATED_INITIALIZER;
- (instancetype)initWithControllerClass:(__unsafe_unretained Class )controllerClass method:(CRHTTPMethod)method path:(NSString * _Nullable)path recursive:(BOOL)recursive;
- (instancetype)initWithViewControllerClass:(__unsafe_unretained Class )viewControllerClass nibName:(NSString * _Nullable)nibNameOrNil bundle:(NSBundle * _Nullable)nibBundleOrNil method:(CRHTTPMethod)method path:(NSString * _Nullable)path recursive:(BOOL)recursive;

- (instancetype)initWithStaticDirectoryAtPath:(NSString *)directoryPath options:(CRStaticDirectoryServingOptions)options path:(NSString * _Nullable)path;
- (instancetype)initWithStaticFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition path:(NSString * _Nullable)path;

@end

NS_ASSUME_NONNULL_END
