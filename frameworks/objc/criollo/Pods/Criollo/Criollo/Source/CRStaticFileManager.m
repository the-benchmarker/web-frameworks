//
//  CRStaticFileManager.m
//  Criollo
//
//  Created by Cătălin Stan on 10/03/16.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRStaticFileManager.h"
#import "CRServer.h"
#import "CRServer_Internal.h"
#import "CRRequest.h"
#import "CRRequest_Internal.h"
#import "CRResponse.h"
#import "CRResponse_Internal.h"
#import "CRConnection.h"
#import "CRConnection_Internal.h"
#import "CRMimeTypeHelper.h"
#import "CRRequestRange.h"

#define CRStaticFileServingReadBuffer                              (8 * 1024 * 1024)
#define CRStaticFileServingReadThreshold                           (8 * 64 * 1024)

#define CRStaticFileManagerErrorDomain                             @"CRStaticFileManagerErrorDomain"

#define CRStaticFileManagerReleaseFailedError                      101
#define CRStaticFileManagerFileReadError                           102
#define CRStaticFileManagerFileIsDirectoryError                    103

#define CRStaticFileManagerRestrictedFileTypeError                 201
#define CRStaticFileManagerRangeNotSatisfiableError                202

#define CRStaticFileManagerNotImplementedError                     999

NS_ASSUME_NONNULL_BEGIN

static NSString * CRStaticFileContentDispositionNoneValue = @"none";
static NSString * CRStaticFileContentDispositionInlineValue = @"inline";
static NSString * CRStaticFileContentDispositionAttachmentValue = @"attachment";

NSString * NSStringFromCRStaticFileContentDisposition(CRStaticFileContentDisposition contentDisposition) {
    switch (contentDisposition) {
        case CRStaticFileContentDispositionNone:
            return CRStaticFileContentDispositionNoneValue;
        case CRStaticFileContentDispositionInline:
            return CRStaticFileContentDispositionInlineValue;
        case CRStaticFileContentDispositionAttachment:
            return CRStaticFileContentDispositionAttachmentValue;
    }
}

CRStaticFileContentDisposition CRStaticFileContentDispositionMake(NSString * contentDispositionName) {
    CRStaticFileContentDisposition contentDisposition;
    if ( [contentDispositionName isEqualToString:CRStaticFileContentDispositionInlineValue] ) {
        contentDisposition = CRStaticFileContentDispositionInline;
    } else if ( [contentDispositionName isEqualToString:CRStaticFileContentDispositionAttachmentValue] ) {
        contentDisposition = CRStaticFileContentDispositionAttachment;
    } else {
        contentDisposition = CRStaticFileContentDispositionNone;
    }
    return contentDisposition;
}

@interface CRStaticFileManager ()

@property (nonatomic, readonly) CRStaticFileServingOptions options;
@property (nonatomic, readonly, strong) dispatch_queue_t fileReadingQueue;

+ (CRRouteBlock)errorHandlerBlockForError:(NSError *)error;
+ (CRRouteBlock)servingBlockForFileAtPath:(NSString *)filePath fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition fileSize:(NSUInteger)fileSize shouldCache:(BOOL)shouldCache fileReadingQueue:(dispatch_queue_t)fileReadingQueue;

NS_ASSUME_NONNULL_END

@end

@implementation CRStaticFileManager

+ (instancetype)managerWithFileAtPath:(NSString *)filePath {
    return [[CRStaticFileManager alloc] initWithFileAtPath:filePath options:0 fileName:nil contentType:nil contentDisposition:CRStaticFileContentDispositionNone attributes:nil];
}

+ (instancetype)managerWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options {
    return [[CRStaticFileManager alloc] initWithFileAtPath:filePath options:options fileName:nil contentType:nil contentDisposition:CRStaticFileContentDispositionNone attributes:nil];
}

+ (instancetype)managerWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName {
    return [[CRStaticFileManager alloc] initWithFileAtPath:filePath options:options fileName:fileName contentType:nil contentDisposition:CRStaticFileContentDispositionNone attributes:nil];
}

+ (instancetype)managerWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType {
    return [[CRStaticFileManager alloc] initWithFileAtPath:filePath options:options fileName:fileName contentType:contentType contentDisposition:CRStaticFileContentDispositionNone attributes:nil];
}

+ (instancetype)managerWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition {
    return [[CRStaticFileManager alloc] initWithFileAtPath:filePath options:options fileName:fileName contentType:contentType contentDisposition:contentDisposition attributes:nil];
}

+ (instancetype)managerWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition attributes:(NSDictionary * _Nullable)attributes {
    return [[CRStaticFileManager alloc] initWithFileAtPath:filePath options:options fileName:fileName contentType:contentType contentDisposition:contentDisposition attributes:attributes];
}

- (instancetype)init {
    return  [self initWithFileAtPath:@"" options:0 fileName:nil contentType:nil contentDisposition:CRStaticFileContentDispositionNone attributes:nil];
}

- (instancetype)initWithFileAtPath:(NSString *)filePath {
    return [self initWithFileAtPath:filePath options:0 fileName:nil contentType:nil contentDisposition:CRStaticFileContentDispositionNone attributes:nil];
}

- (instancetype)initWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options {
    return [self initWithFileAtPath:filePath options:options fileName:nil contentType:nil contentDisposition:CRStaticFileContentDispositionNone attributes:nil];
}

- (instancetype)initWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName {
    return [self initWithFileAtPath:filePath options:options fileName:fileName contentType:nil contentDisposition:CRStaticFileContentDispositionNone attributes:nil];
}

- (instancetype)initWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType {
    return [self initWithFileAtPath:filePath options:options fileName:fileName contentType:contentType contentDisposition:CRStaticFileContentDispositionNone attributes:nil];
}

- (instancetype)initWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition {
    return [self initWithFileAtPath:filePath options:options fileName:fileName contentType:contentType contentDisposition:contentDisposition attributes:nil];
}

- (instancetype)initWithFileAtPath:(NSString *)filePath options:(CRStaticFileServingOptions)options fileName:(NSString * _Nullable)fileName contentType:(NSString *)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition attributes:(NSDictionary *)attributes {

    self = [super init];
    if ( self != nil ) {

        // Sanitize path
        _filePath = filePath.stringByStandardizingPath;

        // Initialize convenience properties
        _options = options;
        _shouldCache = _options & CRStaticFileServingOptionsCache;
        _shouldFollowSymLinks = _options & CRStaticFileServingOptionsFollowSymlinks;

        // Expand symlinks if needed
        if ( _shouldFollowSymLinks ) {
            _filePath = [_filePath stringByResolvingSymlinksInPath];
        }

        if ( fileName ) {
            _fileName = fileName;
        } else {
            _fileName = filePath.lastPathComponent;
        }

        if ( contentType ) {
            _contentType = contentType;
        } else {
            _contentType = [[CRMimeTypeHelper sharedHelper] mimeTypeForFileAtPath:_filePath];
        }

        if ( contentDisposition != CRStaticFileContentDispositionNone ) {
            _contentDisposition = contentDisposition;
        } else {
            _contentDisposition = [_contentType isEqualToString:@"application/octet-stream"] ? CRStaticFileContentDispositionAttachment : CRStaticFileContentDispositionInline;
        }

        // Initialize the attributes
        if ( attributes ) {
            _attributes = attributes;
        } else {
            NSError* attributesError;
            _attributes = [[NSFileManager defaultManager] attributesOfItemAtPath:_filePath error:&attributesError];
            if ( attributesError ) {
                _attributesError = attributesError;
            }
        }

        // Create and configure queues
        NSString* fileReadingQueueLabel = [NSString stringWithFormat:@"%@-%@-fileReadngQueue", NSStringFromClass(self.class), _fileName];
        _fileReadingQueue = dispatch_queue_create(fileReadingQueueLabel.UTF8String, DISPATCH_QUEUE_SERIAL);
        dispatch_set_target_queue(_fileReadingQueue, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0));

        NSString * fileType = _attributes == nil || _attributesError != nil ? nil : _attributes.fileType;
        NSUInteger fileSize = @(_attributes.fileSize).integerValue;

        CRStaticFileManager * __weak manager = self;
        _routeBlock = ^(CRRequest * _Nonnull request, CRResponse * _Nonnull response, CRRouteCompletionBlock  _Nonnull completionHandler) {
            @autoreleasepool {
                if ( !fileType ) {

                    [CRStaticFileManager errorHandlerBlockForError:manager.attributesError](request, response, completionHandler);

                } else if ( [fileType isEqualToString:NSFileTypeDirectory] ) {

                    NSMutableDictionary* userInfo = [NSMutableDictionary dictionary];
                    userInfo[NSLocalizedDescriptionKey] = NSLocalizedString(@"The requested file is a directory.",);
                    userInfo[NSURLErrorFailingURLErrorKey] = request.URL;
                    userInfo[NSFilePathErrorKey] = manager.filePath;
                    NSError* fileIsDirectoryError = [NSError errorWithDomain:CRStaticFileManagerErrorDomain code:CRStaticFileManagerFileIsDirectoryError userInfo:userInfo];
                    [CRStaticFileManager errorHandlerBlockForError:fileIsDirectoryError](request, response, completionHandler);

                } else if ( [fileType isEqualToString:NSFileTypeRegular] ) {

                    [CRStaticFileManager servingBlockForFileAtPath:manager.filePath fileName:manager.fileName contentType:manager.contentType contentDisposition:manager.contentDisposition fileSize:fileSize shouldCache:manager.shouldCache fileReadingQueue:manager.fileReadingQueue](request, response, completionHandler);

                } else {

                    NSMutableDictionary* userInfo = [NSMutableDictionary dictionary];
                    userInfo[NSLocalizedDescriptionKey] = [NSString stringWithFormat:NSLocalizedString(@"Files of type “%@” are restricted.",), fileType];
                    userInfo[NSURLErrorFailingURLErrorKey] = request.URL;
                    userInfo[NSFilePathErrorKey] = manager.filePath;
                    NSError* restrictedFileTypeError = [NSError errorWithDomain:CRStaticFileManagerErrorDomain code:CRStaticFileManagerRestrictedFileTypeError userInfo:userInfo];
                    [CRStaticFileManager errorHandlerBlockForError:restrictedFileTypeError](request, response, completionHandler);
                    
                }
            }
        };
    }
    return self;
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
            } else if ([error.domain isEqualToString:CRStaticFileManagerErrorDomain] ) {
                switch ( error.code ) {
                    case CRStaticFileManagerNotImplementedError:
                        statusCode = 501;
                        break;
                    case CRStaticFileManagerRangeNotSatisfiableError:
                        statusCode = 416;
                        break;
                    default:
                        break;
                }
            }

            [CRRouter errorHandlingBlockWithStatus:statusCode error:error](request, response, completionHandler);
        }
    };
}

+ (CRRouteBlock)servingBlockForFileAtPath:(NSString *)filePath fileName:(NSString * _Nullable)fileName contentType:(NSString * _Nullable)contentType contentDisposition:(CRStaticFileContentDisposition)contentDisposition fileSize:(NSUInteger)fileSize shouldCache:(BOOL)shouldCache fileReadingQueue:(nonnull dispatch_queue_t)fileReadingQueue {
    return ^(CRRequest * _Nonnull request, CRResponse * _Nonnull response, CRRouteCompletionBlock  _Nonnull completionHandler) {
        @autoreleasepool {
            // Send an unimplemented error if we are being requested to serve multipart byte-ranges
            if ( request.range.byteRangeSet.count > 1 ) {
                NSMutableDictionary* userInfo = [NSMutableDictionary dictionary];
                userInfo[NSLocalizedDescriptionKey] = NSLocalizedString(@"Multiple range (multipart/byte-range) responses are not implemented.",);
                userInfo[NSURLErrorFailingURLErrorKey] = request.URL;
                userInfo[NSFilePathErrorKey] = filePath;
                NSError* rangeError = [NSError errorWithDomain:CRStaticFileManagerErrorDomain code:CRStaticFileManagerNotImplementedError userInfo:userInfo];
                [CRStaticFileManager errorHandlerBlockForError:rangeError](request, response, completionHandler);
                return;
            }

            // We are accepting byte ranges
            [response setValue:[CRRequestRange acceptRangesSpec] forHTTPHeaderField:@"Accept-Ranges"];

            CRRequestByteRange* requestByteRange;
            NSRange byteRangeDataRange = NSMakeRange(NSNotFound, 0);

            // Set the Content-length and Content-range headers
            if ( request.range.byteRangeSet.count > 0 ) {

                requestByteRange = request.range.byteRangeSet[0];
                byteRangeDataRange = [requestByteRange dataRangeForFileSize:fileSize];

                NSString* contentRangeSpec = [requestByteRange contentRangeSpecForFileSize:fileSize];
                contentRangeSpec = [NSString stringWithFormat:@"%@ %@", request.range.bytesUnit, contentRangeSpec];
                [response setValue:contentRangeSpec forHTTPHeaderField:@"Content-Range"];

                if ( [request.range isSatisfiableForFileSize:fileSize ] ) {                                // Set partial content response header
                    if ( byteRangeDataRange.location == 0 && byteRangeDataRange.length == fileSize ) {
                        [response setStatusCode:200 description:nil];
                    } else {
                        [response setStatusCode:206 description:nil];
                    }
                    NSString* conentLengthSpec = [requestByteRange contentLengthSpecForFileSize:fileSize];
                    [response setValue:conentLengthSpec forHTTPHeaderField:@"Content-Length"];
                } else {
                    NSMutableDictionary* userInfo = [NSMutableDictionary dictionary];
                    userInfo[NSLocalizedDescriptionKey] = [NSString stringWithFormat:NSLocalizedString(@"The requested byte-range %@-%@ / %lu could not be satisfied.",), requestByteRange.firstBytePos, requestByteRange.lastBytePos, fileSize];
                    userInfo[NSURLErrorFailingURLErrorKey] = request.URL;
                    userInfo[NSFilePathErrorKey] = filePath;
                    NSError* rangeError = [NSError errorWithDomain:CRStaticFileManagerErrorDomain code:CRStaticFileManagerRangeNotSatisfiableError userInfo:userInfo];
                    [CRStaticFileManager errorHandlerBlockForError:rangeError](request, response, completionHandler);
                    return;
                }

            } else {
                [response setValue:@(fileSize).stringValue forHTTPHeaderField:@"Content-Length"];
            }

            // Set Content-Type and Content-Disposition
            [response setValue:contentType forHTTPHeaderField:@"Content-Type"];
            NSString* contentDispositionSpec = [NSString stringWithFormat:@"%@; filename=\"%@\"", NSStringFromCRStaticFileContentDisposition(contentDisposition), [fileName stringByReplacingOccurrencesOfString:@"\"" withString:@"\\\""]];
            [response setValue:contentDispositionSpec forHTTPHeaderField:@"Content-Disposition"];

            // Read synchroniously if the file size is below threshold
            if ( fileSize <= CRStaticFileServingReadThreshold ) {

                NSError* fileReadError;
                NSData* fileData = [NSData dataWithContentsOfFile:filePath options:(shouldCache ? NSDataReadingMappedIfSafe : NSDataReadingUncached) error:&fileReadError];
                if ( fileData == nil && fileReadError != nil ) {
                    [CRStaticFileManager errorHandlerBlockForError:fileReadError](request, response, completionHandler);
                } else {
                    if ( request.range.byteRangeSet.count == 0 ) {
                        [response sendData:fileData];
                    } else {
                        NSData* requestedRangeData = [NSData dataWithBytesNoCopy:(void *)fileData.bytes + byteRangeDataRange.location length:byteRangeDataRange.length freeWhenDone:NO];
                        [response sendData:requestedRangeData];
                    }
                    completionHandler();
                }

            } else {

                dispatch_io_t fileReadChannel = dispatch_io_create_with_path(DISPATCH_IO_RANDOM, filePath.UTF8String, O_RDONLY, 0, fileReadingQueue,  ^(int error) {
                    @autoreleasepool {
                        if ( error ) {
                            NSMutableDictionary* userInfo = [NSMutableDictionary dictionary];
                            userInfo[NSLocalizedDescriptionKey] = NSLocalizedString(@"There was an error releasing the file read channel.",);
                            userInfo[NSURLErrorFailingURLErrorKey] = request.URL;
                            userInfo[NSFilePathErrorKey] = filePath;
                            NSString* underlyingErrorDescription = [NSString stringWithCString:strerror(error) encoding:NSUTF8StringEncoding];
                            if ( underlyingErrorDescription.length > 0 ) {
                                NSError* underlyingError = [NSError errorWithDomain:NSPOSIXErrorDomain code:@(error).integerValue userInfo:@{NSLocalizedDescriptionKey: underlyingErrorDescription}];
                                userInfo[NSUnderlyingErrorKey] = underlyingError;
                            }
                            NSError* channelReleaseError = [NSError errorWithDomain:CRStaticFileManagerErrorDomain code:CRStaticFileManagerFileReadError userInfo:userInfo];
                            [CRStaticFileManager errorHandlerBlockForError:channelReleaseError](request, response, completionHandler);
                            return;
                        }
                        
                        completionHandler();
                        [response finish];
                    }
                });

                dispatch_io_set_high_water(fileReadChannel, CRStaticFileServingReadBuffer);
                dispatch_io_set_low_water(fileReadChannel, CRStaticFileServingReadThreshold);

                off_t offset = 0;
                size_t length = fileSize;
                if ( request.range.byteRangeSet.count > 0 ) {
                    offset = byteRangeDataRange.location;
                    length = byteRangeDataRange.length;
                }

                dispatch_io_read(fileReadChannel, offset, length, fileReadingQueue, ^(bool done, dispatch_data_t data, int error) {
                    @autoreleasepool {
                        if (request.connection == nil || response.connection == nil) {
                            dispatch_io_close(fileReadChannel, DISPATCH_IO_STOP);
                            return;
                        }

                        if ( error ) {
                            NSMutableDictionary* userInfo = [NSMutableDictionary dictionary];
                            userInfo[NSLocalizedDescriptionKey] = NSLocalizedString(@"There was an error releasing the file read channel.",);
                            userInfo[NSURLErrorFailingURLErrorKey] = request.URL;
                            userInfo[NSFilePathErrorKey] = filePath;
                            NSString* underlyingErrorDescription = [NSString stringWithCString:strerror(error) encoding:NSUTF8StringEncoding];
                            if ( underlyingErrorDescription.length > 0 ) {
                                NSError* underlyingError = [NSError errorWithDomain:NSPOSIXErrorDomain code:@(error).integerValue userInfo:@{NSLocalizedDescriptionKey: underlyingErrorDescription}];
                                userInfo[NSUnderlyingErrorKey] = underlyingError;
                            }
                            NSError* fileReadError = [NSError errorWithDomain:CRStaticFileManagerErrorDomain code:CRStaticFileManagerReleaseFailedError userInfo:userInfo];
                            [CRStaticFileManager errorHandlerBlockForError:fileReadError](request, response, completionHandler);
                            return;
                        }

                        if (data) {
                            [response writeData:(NSData*)data];
                        }

                        if (done) {
                            dispatch_io_close(fileReadChannel, 0);
                        }
                    }
                });
            }
        }
    };
}

@end
