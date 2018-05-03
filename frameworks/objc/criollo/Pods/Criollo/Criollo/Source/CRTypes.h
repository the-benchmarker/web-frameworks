//
//  CRTypes.h
//  Criollo
//
//  Created by Cătălin Stan on 11/20/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#define CRPathSeparator                     @"/"
#define CRPathAnyPath                       @"*"

NS_ASSUME_NONNULL_BEGIN

/**
 *  The HTTP version identifiers.
 *
 *  See [RFC2616 section 3.1](https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.1).
 */
typedef NS_ENUM(NSUInteger, CRHTTPVersion) {
    /**
     *  HTTP version 1.0. (HTTP/1.0)
     */
    CRHTTPVersion1_0,
    /**
     *  HTTP version 1.1. (HTTP/1.1)
     */
    CRHTTPVersion1_1,
    /**
     *  HTTP version 2.0. (HTTP/2.0)
     */
    //    CRHTTPVersion2_0,
};

/**
 *  The HTTP request moethods.
 *
 *  @see [RFC2616 section 9](https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html)
 */
typedef NS_ENUM(NSUInteger, CRHTTPMethod) {
    /**
     *  An abstraction for an unrecognized request method. (NONE)
     */
    CRHTTPMethodNone,
    /**
     *  The GET HTTP request method. (GET)
     */
    CRHTTPMethodGet,
    /**
     *  The POST HTTP request method. (Post)
     */
    CRHTTPMethodPost,
    /**
     *  The PUT HTTP request method. (PUT)
     */
    CRHTTPMethodPut,
    /**
     *  The DELETE HTTP request method. (DELETE)
     */
    CRHTTPMethodDelete,
    /**
     *  The PATCH HTTP request method. (PATCH)
     */
    CRHTTPMethodPatch,
    /**
     *  The OPTIONS HTTP request method. (OPTIONS)
     */
    CRHTTPMethodOptions,
    /**
     *  The HEAD HTTP request method. (HEAD)
     */
    CRHTTPMethodHead,
    /**
     *  An abstraction for all/any HTTP request method. (ALL)
     */
    CRHTTPMethodAll,
};

@class CRRequest, CRResponse;

/**
 *  A block that gets called at the end of a [route block](CRRouteBlock),
 *  indicating that execution of the next block can proceed safely.
 */
typedef dispatch_block_t CRRouteCompletionBlock;

/**
 *  A route block is attached to a route for a specified path and HTTP request 
 *  method. It is run as part of the route traversal process.
 *
 *  Blocks are added using the `[CRServer addBlock:]` family of functions.
 *
 *  @param request           The `CRRequest` object for which the block is being
 * executed
 *  @param response          The `CRResponse` object being sent back
 *  @param completionHandler The `CRRouteCompletionBlock` that must be called to
 * pass execution on to the next `CRRouteBlock`
 */
typedef void(^CRRouteBlock)(CRRequest * request, CRResponse * response, CRRouteCompletionBlock completionHandler);

/**
 *  Options for mounting static directories.
 */
typedef NS_OPTIONS(NSUInteger, CRStaticDirectoryServingOptions) {
    /**
     *  Files are cached to the os disk cache. Currently, this option only 
     *  applies to files smaller than 512KB in size.
     *
     *  @see `NSDataReadingMappedIfSafe`.
     */
    CRStaticDirectoryServingOptionsCacheFiles               = 1 <<   0,
    /**
     *  Generate an HTML index for the directory's contents.
     */
    CRStaticDirectoryServingOptionsAutoIndex                = 1 <<   1,
    /**
     *  Show hidden files in the auto-generated directory index.
     */
    CRStaticDirectoryServingOptionsAutoIndexShowHidden      = 1 <<   2,
    /**
     *
     */
    CRStaticDirectoryServingOptionsFollowSymlinks           = 1 <<   3,
};

/**
 *  Options for serving static files from disk.
 */
typedef NS_OPTIONS(NSUInteger, CRStaticFileServingOptions) {
    /**
     *  Files are cached to the os disk cache. Currently, this option only
     *  applies to files smaller than 512KB in size.
     *
     *  @see `NSDataReadingMappedIfSafe`.
     */
    CRStaticFileServingOptionsCache             = 1 <<   0,
    /**
     *  Follow symbolic links.
     */
    CRStaticFileServingOptionsFollowSymlinks    = 1 <<   3,
};

/**
 *  The HTTP Content-Disposition header specification.
 *
 *  @see [RFC2616 section 19.5.1](https://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1)
 */
typedef NS_ENUM(NSUInteger, CRStaticFileContentDisposition) {
    /**
     *  An abstraction for no content disposition specification
     */
    CRStaticFileContentDispositionNone,
    /**
     *  Inline content disposition. Files are served inline. ("inline")
     */
    CRStaticFileContentDispositionInline,
    /**
     *  Attachment content disposition. Force a download. ("attachment")
     */
    CRStaticFileContentDispositionAttachment
};

NS_ASSUME_NONNULL_END
