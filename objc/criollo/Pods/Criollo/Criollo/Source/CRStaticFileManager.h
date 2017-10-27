//
//  CRStaticFileManager.h
//  Criollo
//
//  Created by Cătălin Stan on 10/03/16.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRTypes.h"

NS_ASSUME_NONNULL_BEGIN

FOUNDATION_EXPORT NSString * NSStringFromCRStaticFileContentDisposition(CRStaticFileContentDisposition contentDisposition);
FOUNDATION_EXPORT CRStaticFileContentDisposition CRStaticFileContentDispositionMake(NSString * contentDispositionName);

@interface CRStaticFileManager : NSObject

@property (nonatomic, readonly) NSString * filePath;
@property (nonatomic, readonly) NSDictionary * attributes;
@property (nonatomic, readonly, strong, nullable) NSError* attributesError;
@property (nonatomic, readonly, copy) CRRouteBlock routeBlock;

@property (nonatomic, readonly) BOOL shouldCache;
@property (nonatomic, readonly) BOOL shouldFollowSymLinks;

@property (nonatomic, strong) NSString* fileName;
@property (nonatomic, strong) NSString* contentType;
@property (nonatomic) CRStaticFileContentDisposition contentDisposition;

+ (instancetype)managerWithFileAtPath:(NSString *)filePath;
+ (instancetype)managerWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options;
+ (instancetype)managerWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName;
+ (instancetype)managerWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType;
+ (instancetype)managerWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition;
+ (instancetype)managerWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition attributes:(NSDictionary * _Nullable)attributes;

- (instancetype)initWithFileAtPath:(NSString *)filePath;
- (instancetype)initWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options;
- (instancetype)initWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName;
- (instancetype)initWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType;
- (instancetype)initWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition;
- (instancetype)initWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition attributes:(NSDictionary * _Nullable)attributes;

NS_ASSUME_NONNULL_END

@end
