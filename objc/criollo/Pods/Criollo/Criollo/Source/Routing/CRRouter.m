//
//  CRRoutingCenter.m
//  Criollo
//
//  Created by Cătălin Stan on 19/07/16.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRRouter.h"
#import "CRRouter_Internal.h"
#import "CRRoute.h"
#import "CRRoute_Internal.h"
#import "CRRouteMatchingResult.h"
#import "CRRouteMatchingResult_Internal.h"
#import "CRServer.h"
#import "CRMessage.h"
#import "CRMessage_Internal.h"
#import "CRRequest.h"
#import "CRRequest_Internal.h"
#import "CRResponse.h"
#import "CRResponse_Internal.h"

NS_ASSUME_NONNULL_BEGIN

@interface CRRouter ()

@property (nonatomic, strong, readonly) NSMutableArray<CRRoute *> * routes;

@end

NS_ASSUME_NONNULL_END

@implementation CRRouter

+ (CRRouteBlock)errorHandlingBlockWithStatus:(NSUInteger)statusCode error:(NSError *)error {
    return ^(CRRequest *request, CRResponse *response, CRRouteCompletionBlock completionHandler) {
        @autoreleasepool {
            [response setStatusCode:statusCode description:nil];
            [response setValue:@"text/plain; charset=utf-8" forHTTPHeaderField:@"Content-type"];

            NSMutableString* responseString = [NSMutableString string];

#if DEBUG
            NSError* err;
            if (error == nil) {
                NSMutableDictionary* mutableUserInfo = [NSMutableDictionary dictionaryWithCapacity:2];
                NSString* errorDescription;
                switch (statusCode) {
                    case 404:
                        errorDescription = [NSString stringWithFormat:NSLocalizedString(@"No routes defined for “%@%@%@”",), NSStringFromCRHTTPMethod(request.method), request.URL.path, [request.URL.path hasSuffix:CRPathSeparator] ? @"" : CRPathSeparator];
                        break;
                }
                if ( errorDescription ) {
                    mutableUserInfo[NSLocalizedDescriptionKey] = errorDescription;
                }
                mutableUserInfo[NSURLErrorFailingURLErrorKey] = request.URL;
                err = [NSError errorWithDomain:CRServerErrorDomain code:statusCode userInfo:mutableUserInfo];
            } else {
                err = error;
            }

            // Error details
            [responseString appendFormat:@"%@ %lu\n%@\n", err.domain, (long)err.code, err.localizedDescription];

            // Error user-info
            if ( err.userInfo.count > 0 ) {
                [responseString appendString:@"\nUser Info\n"];
                [err.userInfo enumerateKeysAndObjectsUsingBlock:^(id  _Nonnull key, id  _Nonnull obj, BOOL * _Nonnull stop) {
                    [responseString appendFormat:@"%@: %@\n", key, obj];
                }];
            }

            // Stack trace
            [responseString appendString:@"\nStack Trace\n"];
            [[NSThread callStackSymbols] enumerateObjectsUsingBlock:^(NSString * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                [responseString appendFormat:@"%@\n", obj];
            }];
#else
            [responseString appendFormat:@"Cannot %@ %@", NSStringFromCRHTTPMethod(request.method), request.URL.path];
#endif
            
            [response setValue:@(responseString.length).stringValue forHTTPHeaderField:@"Content-Length"];
            [response sendString:responseString];
            
            completionHandler();
        }
    };
}

- (instancetype)init {
    self = [super init];
    if ( self != nil ) {
        _routes = [NSMutableArray array];
        _notFoundBlock = [CRRouter errorHandlingBlockWithStatus:404 error:nil];
    }
    return self;
}

#pragma mark - Block Routes

- (void)add:(CRRouteBlock)block {
    [self add:nil block:block recursive:NO method:CRHTTPMethodAll];
}

- (void)add:(NSString *)path block:(CRRouteBlock)block {
    [self add:path block:block recursive:NO method:CRHTTPMethodAll];
}

- (void)add:(NSString *)path block:(CRRouteBlock)block recursive:(BOOL)recursive method:(CRHTTPMethod)method {
    CRRoute* route = [[CRRoute alloc] initWithBlock:block method:method path:path recursive:recursive];
    [self addRoute:route];
}

- (void)get:(NSString *)path block:(CRRouteBlock)block {
    [self add:path block:block recursive:NO method:CRHTTPMethodGet];
}

- (void)post:(NSString *)path block:(CRRouteBlock)block {
    [self add:path block:block recursive:NO method:CRHTTPMethodPost];
}

- (void)put:(NSString *)path block:(CRRouteBlock)block {
    [self add:path block:block recursive:NO method:CRHTTPMethodPut];
}

- (void)delete:(NSString *)path block:(CRRouteBlock)block {
    [self add:path block:block recursive:NO method:CRHTTPMethodDelete];
}

- (void)head:(NSString *)path block:(CRRouteBlock)block {
    [self add:path block:block recursive:NO method:CRHTTPMethodHead];
}

- (void)options:(NSString *)path block:(CRRouteBlock)block {
    [self add:path block:block recursive:NO method:CRHTTPMethodOptions];
}

#pragma mark - Route Controller Routes

- (void)add:(NSString *)path controller:(__unsafe_unretained Class)controllerClass {
    [self add:path controller:controllerClass recursive:YES method:CRHTTPMethodAll];
}

- (void)add:(NSString *)path controller:(__unsafe_unretained Class)controllerClass recursive:(BOOL)recursive method:(CRHTTPMethod)method {
    CRRoute* route = [[CRRoute alloc] initWithControllerClass:controllerClass method:method path:path recursive:recursive];
    [self addRoute:route];
}


#pragma mark - View Controller Routes

- (void)add:(NSString *)path viewController:(__unsafe_unretained Class)viewControllerClass withNibName:(NSString *)nibNameOrNil bundle:(NSBundle*)nibBundleOrNil {
    [self add:path viewController:viewControllerClass withNibName:nibNameOrNil bundle:nibBundleOrNil recursive:YES method:CRHTTPMethodAll];
}

- (void)add:(NSString *)path viewController:(__unsafe_unretained Class)viewControllerClass withNibName:(NSString *)nibNameOrNil bundle:(NSBundle*)nibBundleOrNil recursive:(BOOL)recursive method:(CRHTTPMethod)method {
    CRRoute* route = [[CRRoute alloc] initWithViewControllerClass:viewControllerClass nibName:nibNameOrNil bundle:nibBundleOrNil method:method path:path recursive:recursive];
    [self addRoute:route];
}

#pragma mark - Static file delivery

- (void)mount:(NSString *)path directoryAtPath:(NSString *)directoryPath {
    [self mount:path directoryAtPath:directoryPath options:0];
}

- (void)mount:(NSString *)path directoryAtPath:(NSString *)directoryPath options:(CRStaticDirectoryServingOptions)options {
    CRRoute* route = [[CRRoute alloc] initWithStaticDirectoryAtPath:directoryPath options:options path:path];
    [self addRoute:route];
}

- (void)mount:(NSString *)path fileAtPath:(NSString *)filePath {
    [self mount:path fileAtPath:filePath options:0 fileName:nil contentType:nil contentDisposition:CRStaticFileContentDispositionNone];
}

- (void)mount:(NSString *)path fileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition {
    CRRoute* route = [[CRRoute alloc] initWithStaticFileAtPath:filePath options:options fileName:fileName contentType:contentType contentDisposition:contentDisposition path:path];
    [self addRoute:route];
}


#pragma mark - Routing

- (void)addRoute:(CRRoute*)route {
    [self.routes addObject:route];
}

- (NSArray<CRRouteMatchingResult *> *)routesForPath:(NSString*)path method:(CRHTTPMethod)method {
//    NSLog(@"%s %@ %@", __PRETTY_FUNCTION__, path, NSStringFromCRHTTPMethod(method));
    NSMutableArray<CRRouteMatchingResult *> * routes = [NSMutableArray array];
    [self.routes enumerateObjectsUsingBlock:^(CRRoute * _Nonnull route, NSUInteger idx, BOOL * _Nonnull stop) {
        @autoreleasepool {
            // Bailout early if method does not match
            if ( route.method != method && route.method != CRHTTPMethodAll ) {
                return;
            }

            // Boilout early if route is valid for all paths or path matches exaclty
            if ( route.path == nil || [route.path isEqualToString:path] ) {
                [routes addObject:[CRRouteMatchingResult routeMatchingResultWithRoute:route matches:nil]];
                return;
            }

            // If route is recursive just check that the path start with the route path
            if ( route.recursive && [path hasPrefix:route.path] ) {
                [routes addObject:[CRRouteMatchingResult routeMatchingResultWithRoute:route matches:nil]];
                return;
            }

            // If the route regex matches
            if ( !route.pathRegex ) {
                return;
            }

            NSArray* matches = [route processMatchesInPath:path];
            if ( matches.count > 0 ) {
                [routes addObject:[CRRouteMatchingResult routeMatchingResultWithRoute:route matches:matches]];
            }
        }
    }];
    return routes;
}

- (void)executeRoutes:(NSArray<CRRouteMatchingResult *> *)routes forRequest:(CRRequest *)request response:(CRResponse *)response withCompletion:(nonnull CRRouteCompletionBlock)completionBlock {
    [self executeRoutes:routes forRequest:request response:response withCompletion:completionBlock notFoundBlock:nil];
}

- (void)executeRoutes:(NSArray<CRRouteMatchingResult *> *)routes forRequest:(CRRequest *)request response:(CRResponse *)response withCompletion:(nonnull CRRouteCompletionBlock)completionBlock notFoundBlock:(CRRouteBlock _Nullable)notFoundBlock {
    if ( !notFoundBlock ) {
        notFoundBlock = [CRRouter errorHandlingBlockWithStatus:404 error:nil];
    }

    if ( routes.count == 0 ) {
        CRRoute* defaultRoute = [[CRRoute alloc] initWithBlock:notFoundBlock method:CRHTTPMethodAll path:nil recursive:NO];
        routes = @[[CRRouteMatchingResult routeMatchingResultWithRoute:defaultRoute matches:nil]];
    }

    __block BOOL shouldStopExecutingBlocks = NO;
    __block NSUInteger currentRouteIndex = 0;
    while (!shouldStopExecutingBlocks && currentRouteIndex < routes.count ) {
        shouldStopExecutingBlocks = YES;
        CRRouteMatchingResult* result = routes[currentRouteIndex];
        if ( result.matches.count > 0 ) {
            [result.route.pathKeys enumerateObjectsUsingBlock:^(NSString * _Nonnull key, NSUInteger idx, BOOL * _Nonnull stop) {
                [request setQuery:(result.matches[idx] ? : @"") forKey:key];
            }];            
        }
        CRRouteBlock routeBlock = result.route.block;
        routeBlock (request, response, ^{
            shouldStopExecutingBlocks = NO;
            currentRouteIndex++;
            if ( currentRouteIndex == routes.count ) {
                if ( !response.finished ) {
                    if ( !response.hasWrittenBodyData ) {
                        notFoundBlock(request, response, ^{
                            if ( !response.finished) {
                                [response finish];
                            }
                            if ( completionBlock ) {
                                completionBlock();
                            }
                        });
                    } else {
                        [response finish];
                        if ( completionBlock ) {
                            completionBlock();
                        }
                    }
                } else if ( completionBlock ) {
                    completionBlock();
                }
            }
        });
    }
}

@end
