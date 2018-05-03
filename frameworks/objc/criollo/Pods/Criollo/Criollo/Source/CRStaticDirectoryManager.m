//
//  CRStaticDirectoryManager.m
//  Criollo
//
//  Created by Cătălin Stan on 2/10/16.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRStaticDirectoryManager.h"
#import "CRStaticFileManager.h"
#import "CRServer.h"
#import "CRServer_Internal.h"
#import "CRRequest.h"
#import "CRResponse.h"
#import "NSString+Criollo.h"

#define CRStaticDirectoryIndexFileNameLength                            70
#define CRStaticDirectoryIndexFileSizeLength                            20

#define CRStaticDirectoryManagerErrorDomain                             @"CRStaticDirectoryManagerErrorDomain"
#define CRStaticDirectoryManagerDirectoryListingForbiddenError          201
#define CRStaticDirectoryManagerNotImplementedError                     999

@interface CRStaticDirectoryManager ()

NS_ASSUME_NONNULL_BEGIN

@property (nonatomic, readonly) NSString * prefix;
@property (nonatomic, readonly) CRStaticDirectoryServingOptions options;

+ (CRRouteBlock)errorHandlerBlockForError:(NSError *)error;
+ (CRRouteBlock)indexBlockForDirectoryAtPath:(NSString *)directoryPath requestedPath:(NSString *)requestedPath prefix:(NSString *)prefix displayParentLink:(BOOL)displayParentLink showHiddenFiles:(BOOL)showHiddenFiles;

+ (NSDateFormatter *)dateFormatter;

NS_ASSUME_NONNULL_END

@end

@implementation CRStaticDirectoryManager {
    CRRouteBlock _routeBlock;
}

static const NSDateFormatter *dateFormatter;

+ (void)initialize {
    dateFormatter = [[NSDateFormatter alloc] init];
    dateFormatter.calendar = [NSCalendar calendarWithIdentifier:NSCalendarIdentifierGregorian];
    dateFormatter.timeZone = [NSTimeZone defaultTimeZone];
    dateFormatter.dateFormat = @"dd-MMM-yyyy HH:mm:ss";
}

+ (instancetype)managerWithDirectoryAtPath:(NSString *)directoryPath prefix:(NSString *)prefix {
    return [[CRStaticDirectoryManager alloc] initWithDirectoryAtPath:directoryPath prefix:prefix options:0];
}

+ (instancetype)managerWithDirectoryAtPath:(NSString *)directoryPath prefix:(NSString *)prefix options:(CRStaticDirectoryServingOptions)options {
    return [[CRStaticDirectoryManager alloc] initWithDirectoryAtPath:directoryPath prefix:prefix options:options];
}

- (instancetype)init {
    return  [self initWithDirectoryAtPath:[NSBundle mainBundle].bundlePath prefix:CRPathSeparator options:0];
}

- (instancetype)initWithDirectoryAtPath:(NSString *)directoryPath prefix:(NSString *)prefix {
    return [self initWithDirectoryAtPath:directoryPath prefix:prefix options:0];
}

- (instancetype)initWithDirectoryAtPath:(NSString *)directoryPath prefix:(NSString *)prefix options:(CRStaticDirectoryServingOptions)options {
    self = [super init];
    if ( self != nil ) {

        // Sanitize paths
        _directoryPath = directoryPath.stringByStandardizingPath;
        _prefix = prefix.stringByStandardizingPath;

        // Initialize convenience properties
        _options = options;
        _shouldCacheFiles = _options & CRStaticDirectoryServingOptionsCacheFiles;
        _shouldGenerateDirectoryIndex = _options & CRStaticDirectoryServingOptionsAutoIndex;
        _shouldShowHiddenFilesInDirectoryIndex = _options & CRStaticDirectoryServingOptionsAutoIndexShowHidden;
        _shouldFollowSymLinks = _options & CRStaticDirectoryServingOptionsFollowSymlinks;

        CRStaticDirectoryManager * __weak manager = self;
        _routeBlock = ^(CRRequest * _Nonnull request, CRResponse * _Nonnull response, CRRouteCompletionBlock  _Nonnull completionHandler) {
            @autoreleasepool {
                NSString* requestedDocumentPath = request.env[@"DOCUMENT_URI"];
                NSString* requestedRelativePath = [requestedDocumentPath pathRelativeToPath:manager.prefix];
                NSString* requestedAbsolutePath = [[manager.directoryPath stringByAppendingPathComponent:requestedRelativePath] stringByStandardizingPath];

                // Expand symlinks if needed
                if ( manager.shouldFollowSymLinks ) {
                    requestedAbsolutePath = [requestedAbsolutePath stringByResolvingSymlinksInPath];
                }

                // stat() the file
                NSError * itemAttributesError;
                NSDictionary * itemAttributes = [[NSFileManager defaultManager] attributesOfItemAtPath:requestedAbsolutePath error:&itemAttributesError];
                if ( itemAttributes == nil || itemAttributesError != nil ) {
                    // Unable to stat() the file
                    [CRStaticDirectoryManager errorHandlerBlockForError:itemAttributesError](request, response, completionHandler);
                } else if ( [itemAttributes.fileType isEqualToString:NSFileTypeDirectory] ) {
                    if ( manager.shouldGenerateDirectoryIndex ) {
                        // Make the index
                        [CRStaticDirectoryManager indexBlockForDirectoryAtPath:requestedAbsolutePath requestedPath:requestedDocumentPath prefix:manager.prefix displayParentLink:requestedRelativePath.length != 0 showHiddenFiles:manager.shouldShowHiddenFilesInDirectoryIndex](request, response, completionHandler);
                    } else {
                        // Forbidden
                        NSMutableDictionary* userInfo = [NSMutableDictionary dictionary];
                        userInfo[NSLocalizedDescriptionKey] = NSLocalizedString(@"Directory index auto-generation is disabled",);
                        userInfo[NSURLErrorFailingURLErrorKey] = request.URL;
                        userInfo[NSFilePathErrorKey] = requestedAbsolutePath;
                        NSError* directoryListingError = [NSError errorWithDomain:CRStaticDirectoryManagerErrorDomain code:CRStaticDirectoryManagerDirectoryListingForbiddenError userInfo:userInfo];
                        [CRStaticDirectoryManager errorHandlerBlockForError:directoryListingError](request, response, completionHandler);
                    }
                } else {
                    // Serve the file through the StaticFileManager
                    CRStaticFileServingOptions options = 0;
                    if ( manager.shouldCacheFiles ) {
                        options |= CRStaticFileServingOptionsCache;
                    }
                    if ( manager.shouldFollowSymLinks ) {
                        options |= CRStaticFileServingOptionsFollowSymlinks;
                    }
                    CRStaticFileManager* staticFileManager = [CRStaticFileManager managerWithFileAtPath:requestedAbsolutePath options:options fileName:nil contentType:nil contentDisposition:CRStaticFileContentDispositionNone];
                    staticFileManager.routeBlock(request, response, completionHandler);
                }
            }
        };

    }
    return self;
}

+ (NSDateFormatter *)dateFormatter {
    return (NSDateFormatter *)dateFormatter;
}

+ (CRRouteBlock)errorHandlerBlockForError:(NSError *)error {
    return ^(CRRequest * _Nonnull request, CRResponse * _Nonnull response, CRRouteCompletionBlock  _Nonnull completionHandler) {
        @autoreleasepool {
            NSUInteger statusCode = 500;
            if ( [error.domain isEqualToString:NSCocoaErrorDomain] ) {
                switch ( error.code ) {
                    case NSFileReadNoSuchFileError:
                        statusCode = 404;
                        break;
                    case NSFileReadNoPermissionError:
                        statusCode = 403;
                        break;
                    default:
                        break;
                }
            } else if ([error.domain isEqualToString:CRStaticDirectoryManagerErrorDomain] ) {
                switch ( error.code ) {
                    case CRStaticDirectoryManagerNotImplementedError:
                        statusCode = 501;
                        break;
                    default:
                        break;
                }
            }

            [CRRouter errorHandlingBlockWithStatus:statusCode error:error](request, response, completionHandler);
        }
    };
}

+ (CRRouteBlock)indexBlockForDirectoryAtPath:(NSString *)directoryPath requestedPath:(NSString *)requestedPath prefix:(NSString *)prefix displayParentLink:(BOOL)displayParentLink showHiddenFiles:(BOOL)showHiddenFiles {
    return ^(CRRequest * _Nonnull request, CRResponse * _Nonnull response, CRRouteCompletionBlock  _Nonnull completionHandler) {
        @autoreleasepool {
            NSMutableString* responseString = [NSMutableString string];
            [responseString appendString:@"<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"/><meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"/><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>"];
            [responseString appendFormat:@"<title>%@</title>", requestedPath];
            [responseString appendString:@"</head><body>"];
            [responseString appendFormat:@"<h1>Index of %@</h1>", requestedPath];
            [responseString appendString:@"<hr/>"];

            NSError *directoryListingError;
            NSArray<NSURL *> *directoryContents = [[NSFileManager defaultManager] contentsOfDirectoryAtURL:[NSURL fileURLWithPath:directoryPath] includingPropertiesForKeys:nil options:(showHiddenFiles ? 0 : NSDirectoryEnumerationSkipsHiddenFiles) error:&directoryListingError];
            if ( directoryContents == nil && directoryListingError != nil ) {
                [CRStaticDirectoryManager errorHandlerBlockForError:directoryListingError](request, response, completionHandler);
                return;
            }

            [responseString appendString:@"<pre>"];

            if ( displayParentLink && ! [requestedPath hasSuffix:prefix] ) {
                [responseString appendFormat:@"<a href=\"%@\">../</a>\n", requestedPath.stringByDeletingLastPathComponent];
            }

            [directoryContents enumerateObjectsUsingBlock:^(NSURL * _Nonnull URL, NSUInteger idx, BOOL * _Nonnull stop) {
                NSError* attributesError;
                NSDictionary* attributes = [[NSFileManager defaultManager] attributesOfItemAtPath:URL.path error:&attributesError];
                if ( attributes == nil && attributesError != nil ) {
                    NSLog(@"%@", attributesError);
                    return;
                }

                BOOL isDirectory = [attributes.fileType isEqualToString:NSFileTypeDirectory];
                NSString* fullName = [URL.lastPathComponent stringByReplacingOccurrencesOfString:@"\"" withString:@"\\\""];
                NSString* fileName = URL.lastPathComponent;
                NSString* fileNamePadding;
                if ( fileName.length > CRStaticDirectoryIndexFileNameLength ) {
                    fileName = [fileName substringToIndex:CRStaticDirectoryIndexFileNameLength - (isDirectory ? 1 : 0)];
                    fileNamePadding = @"";
                } else {
                    fileNamePadding = [@"" stringByPaddingToLength:CRStaticDirectoryIndexFileNameLength - fileName.length - (isDirectory ? 1 : 0) withString:@" " startingAtIndex:0];
                }

                NSString* fileModificationDate = [[CRStaticDirectoryManager dateFormatter] stringFromDate:attributes.fileModificationDate];
                NSString* fileSize = @(attributes.fileSize).stringValue;
                NSString* fileSizePadding;
                if ( fileSize.length > CRStaticDirectoryIndexFileSizeLength ) {
                    fileSize =  [fileSize substringToIndex:CRStaticDirectoryIndexFileSizeLength];
                    fileSizePadding = @"";
                } else {
                    fileSizePadding = [@"" stringByPaddingToLength:CRStaticDirectoryIndexFileSizeLength - fileSize.length withString:@" " startingAtIndex:0];
                }

                [responseString appendFormat:@"<a href=\"%@%@%@\" title=\"%@\">%@%@</a>%@ %@ %@%@\n", requestedPath, [requestedPath isEqualToString:CRPathSeparator] ? @"" : CRPathSeparator, URL.lastPathComponent, fullName, fileName, isDirectory ? CRPathSeparator : @"", fileNamePadding, fileModificationDate, fileSizePadding, fileSize];
            }];
            [responseString appendString:@"</pre>"];

            [responseString appendString:@"<hr/></body></html>"];
            
            [response setValue:@"text/html; charset=utf-8" forHTTPHeaderField:@"Content-Type"];
            [response setValue:@(responseString.length).stringValue forHTTPHeaderField:@"Content-Length"];
            [response sendString:responseString];
            completionHandler();
        }
    };
}

@end
