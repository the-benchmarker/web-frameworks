//
//  CRMimeTypeHelper.m
//  Criollo
//
//  Created by Cătălin Stan on 2/11/16.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRMimeTypeHelper.h"
#if TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR
#import <MobileCoreServices/MobileCoreServices.h>
#else
#import <CoreServices/CoreServices.h>
#endif

NS_ASSUME_NONNULL_BEGIN
@interface CRMimeTypeHelper ()

@property (nonatomic, strong, readonly) NSMutableDictionary<NSString *, NSString *> *mimeTypes;
@property (nonatomic, strong, readonly) dispatch_queue_t isolationQueue;

@end
NS_ASSUME_NONNULL_END

@implementation CRMimeTypeHelper

static const CRMimeTypeHelper *sharedHelper;

+ (void)initialize {
    sharedHelper = [[CRMimeTypeHelper alloc] init];
}

+ (instancetype)sharedHelper {
    return (CRMimeTypeHelper *)sharedHelper;
}

- (instancetype)init {
    self = [super init];
    if ( self != nil ) {
        _mimeTypes = [NSMutableDictionary dictionary];
        _isolationQueue = dispatch_queue_create([[NSStringFromClass(self.class) stringByAppendingPathExtension:@"IsolationQueue"] cStringUsingEncoding:NSASCIIStringEncoding], DISPATCH_QUEUE_SERIAL);
        dispatch_set_target_queue(_isolationQueue, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0));
    }
    return self;
}

- (NSString *)mimeTypeForExtension:(NSString *)extension {
    return self.mimeTypes[extension];
}

- (void)setMimeType:(NSString *)mimeType forExtension:(NSString *)extension {
    if ( mimeType != nil ) {
        dispatch_async(self.isolationQueue, ^{
            self.mimeTypes[extension] = mimeType;
        });
    }
}

- (NSString *)mimeTypeForFileAtPath:(NSString *)path {
    NSString *fileExtension = path.pathExtension;
    NSString *contentType = [self mimeTypeForExtension:fileExtension];

    if ( contentType.length == 0 ) {

        NSString *UTI = (__bridge_transfer NSString *)UTTypeCreatePreferredIdentifierForTag(kUTTagClassFilenameExtension, (__bridge CFStringRef)fileExtension, NULL);
        contentType = (__bridge_transfer NSString *)UTTypeCopyPreferredTagWithClass((__bridge CFStringRef)UTI, kUTTagClassMIMEType);

        if ( contentType.length == 0 ) {
            if ( UTTypeConformsTo((__bridge CFStringRef _Nonnull)(UTI), kUTTypeText) ) {
                contentType = @"text/plain; charset=utf-8";
            } else if ( UTTypeConformsTo((__bridge CFStringRef _Nonnull)(UTI), kUTTypeSourceCode) ) {
                contentType = @"text/plain; charset=utf-8";
            } else if ( UTTypeConformsTo((__bridge CFStringRef _Nonnull)(UTI), kUTTypeXMLPropertyList) ) {
                contentType = @"application/xml; charset=utf-8";
            } else {
                contentType = @"application/octet-stream; charset=binary";
            }
        }

        if ( UTTypeConformsTo((__bridge CFStringRef _Nonnull)(UTI), kUTTypeText) ) {
            contentType = [contentType stringByAppendingString:@"; charset=utf-8"];
        }

        [self setMimeType:contentType forExtension:fileExtension];

    }

    return contentType;
}


@end
