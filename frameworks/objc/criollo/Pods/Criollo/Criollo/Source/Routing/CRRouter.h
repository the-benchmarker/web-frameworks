//
//  CRRoutingCenter.h
//  Criollo
//
//  Created by Cătălin Stan on 19/07/16.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRTypes.h"

#define CRPathVarsKey       @"vars"

@class CRRoute, CRRouteMatchingResult;

NS_ASSUME_NONNULL_BEGIN

/**
 `CRRouter` provides Criollo's routing capabilities. It is the base class for
 `CRServer`, `CRRouteController` and `CRViewController`. It is not meant to be
 instantiated directly, but rather it's methods should be called through one of
 the decendant classes mentioned above. Instances of these classes are called 
 **routers**.

 ## Routing
 
 In Criollo, a route consists of an ordered list of `CRRouteBlock` objects,
 associated with path and an HTTP request method. Blocks are added directly to a
 route by calling `add:block:recursive:method:`, one of its overloadds or any of
 the convenience methods provided, such as `get:block:`,
 `post:block:recursive:`, etc.

 *Important:* blocks are executed in the order in which they are added.
 
 The final parameter of a `CRRouteBlock` is a `dispatch_block_t` object that 
 must be called in order to signal the router that the work of the cuurent block
 has finished its job and execution can proceed to the next block.
 
 ### Path specification
 
 When adding a route, paths can be specified in three ways:
 
 - *Fixed string* (ex. `/api`). This will match the string exactly.

 - *Placeholders* (ex. `/posts/:pid`). The next path component after "/posts",
 will be matched and added to `request.query` under the `pid` key.

 - *Regex patterns* (ex. `/[0-9]{4}/[0-9]{1,2}/[a-zA-Z0-9-]+`). When the three
 patterns are matched, they are added to `request.query`, under the keys `"0"`,
 `"1"` and `"2"` respectively.
 
 *Important*: Regex patterns are parsed **per path component**.

 The string that is used to define a path is called a **path specification 
 (pathspec)**.
 
 ### Block, Route Controllers and View Controllers
 
 In real-life, the functionality required by a specific route cannot be simply
 implemented by a single block. Not in a readable way anyway. Come in
 `CRRouteController` and `CRViewController`. These two classes are used to
 provide a more apropriate wrapper for routes that share a common logic.
 
 `CRRouteController` and `CRViewController` instances are added to the router by
 calling `add:controller:recursive:method:` and 
 `add:viewController:withNibName:bundle:recursive:method:` or their overloads.

 Both `CRRouteController` and `CRViewController` expose a `CRRouteBlock` as 
 their workhorse.
 
 ### Serving Static Files
 
 `CRRouter` also provides methods for serving files stored in a directory on
 disk (`mount:directoryAtPath:options:`) or for serving a file
 directly at a specific path
 (`mount:fileAtPath:options:fileName:contentType:contentDisposition:`).
 */
@interface CRRouter : NSObject

+ (CRRouteBlock)errorHandlingBlockWithStatus:(NSUInteger)statusCode error:(NSError * _Nullable)error;

/**
 The block called when there are no routes for the requested pathspec
 */
@property (nonatomic, copy) CRRouteBlock notFoundBlock;

/**
 @name Adding a Block
 */

/**
 Adds a block to all pathspecs and all HTTP methods.

 @param block The `CRRouteBlock` to be executed.
 */
- (void)add:(CRRouteBlock)block;

/**
 Adds a block to a pathspec, for all HTTP methods, non-recursively.

 @param path  The path specification.
 @param block The `CRRouteBlock` to be executed.
 */
- (void)add:(NSString * _Nullable)path block:(CRRouteBlock)block;

/**
 Adds a block to a pathspec and HTTP method.

 @param path      The path specification.
 @param block     The `CRRouteBlock` to be executed.
 @param recursive Specifies wether the path spec should be matched exactly or if
 it should be treated aa a prefix.
 @param method    The HTTP request method
 */
- (void)add:(NSString * _Nullable)path block:(CRRouteBlock)block recursive:(BOOL)recursive method:(CRHTTPMethod)method;

/**
 Adds a block to a pathspec, for the GET method, non-recursively.

 @param path  The path specification.
 @param block The `CRRouteBlock` to be executed.
 */
- (void)get:(NSString * _Nullable)path block:(CRRouteBlock)block;

/**
 Adds a block to a pathspec, for the POST method, non-recursively.

 @param path  The path specification.
 @param block The `CRRouteBlock` to be executed.
 */
- (void)post:(NSString * _Nullable)path block:(CRRouteBlock)block;

/**
 Adds a block to a pathspec, for the PUT method, non-recursively.

 @param path  The path specification.
 @param block The `CRRouteBlock` to be executed.
 */
- (void)put:(NSString * _Nullable)path block:(CRRouteBlock)block;

/**
 Adds a block to a pathspec, for the DELETE method, non-recursively.

 @param path  The path specification.
 @param block The `CRRouteBlock` to be executed.
 */
- (void)delete:(NSString * _Nullable)path block:(CRRouteBlock)block;

/**
 Adds a block to a pathspec, for the HEAD method, non-recursively.

 @param path  The path specification.
 @param block The `CRRouteBlock` to be executed.
 */
- (void)head:(NSString * _Nullable)path block:(CRRouteBlock)block;

/**
 Adds a block to a pathspec, for the OPTIONS method, non-recursively.

 @param path  The path specification.
 @param block The `CRRouteBlock` to be executed.
 */
- (void)options:(NSString * _Nullable)path block:(CRRouteBlock)block;

/**
 @name Adding a Controller
 */

/**
 Adds a controller to a pathspec, for all HTTP methods, recursively.

 @param path            The path specification.
 @param controllerClass The class name of the controller to be used.
 */
- (void)add:(NSString *)path controller:(__unsafe_unretained Class)controllerClass;

/**
 Adds a controller to a pathspec and HTTP method.

 @param path            The path specification.
 @param controllerClass The class name of the controller to be used.
 @param recursive       Specifies wether the path spec should be matched exactly or if
 it should be treated aa a prefix.
 @param method          The HTTP request method
 */
- (void)add:(NSString *)path controller:(__unsafe_unretained Class)controllerClass recursive:(BOOL)recursive method:(CRHTTPMethod)method;

/**
 @name Adding a View Controller
 */

/**
 Adds a view controller to a pathspec, for all HTTP methods, recursively.

 @param path                The path specification.
 @param viewControllerClass The class name of the controller to be used.
 @param nibNameOrNil        The name of the `nib` file to associate with the view controller.
 @param nibBundleOrNil      he bundle in which to search for the `nib` file.
 */
- (void)add:(NSString *)path viewController:(__unsafe_unretained Class)viewControllerClass withNibName:(NSString * _Nullable)nibNameOrNil bundle:(NSBundle * _Nullable)nibBundleOrNil;

/**

 Adds a view controller to a pathspec and HTTP method.

 @param path                The path specification.
 @param viewControllerClass The class name of the controller to be used.
 @param nibNameOrNil        The name of the `nib` file to associate with the view controller.
 @param nibBundleOrNil      he bundle in which to search for the `nib` file.
 @param recursive       Specifies wether the path spec should be matched exactly or if
 it should be treated aa a prefix.
 @param method          The HTTP request method
 */
- (void)add:(NSString *)path viewController:(__unsafe_unretained Class)viewControllerClass withNibName:(NSString * _Nullable)nibNameOrNil bundle:(NSBundle * _Nullable)nibBundleOrNil recursive:(BOOL)recursive method:(CRHTTPMethod)method;

/**
 @name Serving Directories
 */

/**
 Exposes the contents of a directory at a path specification.

 @param path            The path specification.
 @param directoryPath   The filesystem path of the directory.
 */
- (void)mount:(NSString *)path directoryAtPath:(NSString *)directoryPath;

/**
 Exposes the contents of a directory at a path specification.

 @param path            The path specification.
 @param directoryPath   The filesystem path of the directory.
 @param options         A bitwise-or'ed list of `CRStaticDirectoryServingOptions`.
 */
- (void)mount:(NSString *)path directoryAtPath:(NSString *)directoryPath options:(CRStaticDirectoryServingOptions)options;

/**
 @name Serving Files
 */

/**
 Exposes the contents of a file at a path specification.
 
 @param path               The path specification.
 @param filePath           The filesystem path of the file.
 */
- (void)mount:(NSString *)path fileAtPath:(NSString *)filePath;

/**
 Exposes the contents of a file at a path specification.

 @param path               The path specification.
 @param filePath           The filesystem path of the file.
 @param options            A bitwise-or'ed list of `CRStaticFileServingOptions`.
 @param fileName           The filename that will be exposed to the client.
 @param contentType        The value of the `Content-type` HTTP header.
 @param contentDisposition The `CRStaticFileContentDisposition` used to build the
 `Content-disposition` HTTP header.
 */
- (void)mount:(NSString *)path fileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition;

@end

NS_ASSUME_NONNULL_END
