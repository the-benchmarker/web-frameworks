//
//  CRUploadedFile.m
//  Criollo
//
//  Created by Cătălin Stan on 1/14/16.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRUploadedFile.h"
#import "CRUploadedFile_Internal.h"
#import "CRApplication.h"
#import "CRMimeTypeHelper.h"
#import "CRRequest.h"

NS_ASSUME_NONNULL_BEGIN

@interface CRUploadedFile ()

@property (nonatomic, strong, nullable) NSOutputStream * fileWriteStream;
@property (nonatomic, readonly) BOOL canCloseStream;
@property (nonatomic) NSUInteger totalBytesToWrite;
@property (nonatomic) NSUInteger bytesWritten;

+ (nullable NSURL *)temporaryFileURL;

@end

NS_ASSUME_NONNULL_END

@implementation CRUploadedFile

- (instancetype)init {
    return [self initWithName:@""];
}

- (instancetype)initWithName:(NSString *)name {
    self = [super init];
    if ( self != nil ) {
        self.name = name;
        self.temporaryFileURL = [CRUploadedFile temporaryFileURL];
    }
    return self;
}

- (BOOL)canCloseChannel {
    return self.totalBytesToWrite != 0 && self.totalBytesToWrite == self.bytesWritten;
}

- (void)fetchAttributes {
    NSError * error;
    self.attributes = [[NSFileManager defaultManager] attributesOfItemAtPath:self.temporaryFileURL.path error:&error];
    if ( !self.attributes ) {
        [CRApp logErrorFormat:@"Unable to fetch attributes of %@: %@", self.temporaryFileURL, error];
    }
}

- (void)fetchMimeType {
    self.mimeType = [[CRMimeTypeHelper sharedHelper] mimeTypeForFileAtPath:self.temporaryFileURL.path];
}

- (void)appendData:(NSData *)data {
    if ( self.fileWriteStream == nil ) {
        self.fileWriteStream = [NSOutputStream outputStreamToFileAtPath:self.temporaryFileURL.path append:YES];
        [self.fileWriteStream open];
    }

    self.totalBytesToWrite += data.length;
    NSInteger bytesWritten = [self.fileWriteStream write:data.bytes maxLength:data.length];
    if ( bytesWritten > 0 ) {
        self.bytesWritten += bytesWritten;
    } else {
        NSMutableDictionary* userInfo = [NSMutableDictionary dictionary];
        userInfo[NSLocalizedDescriptionKey] = NSLocalizedString(@"There was an error writing to the temporary file.",);
        userInfo[NSFilePathErrorKey] = self.temporaryFileURL.path;
        userInfo[NSUnderlyingErrorKey] = self.fileWriteStream.streamError;
        NSError* fileWriteError = [NSError errorWithDomain:CRRequestErrorDomain code:CRRequestFileWriteError userInfo:userInfo];
        [CRApp logErrorFormat:@"%@: %@", fileWriteError.localizedDescription, fileWriteError];
    }
}

- (void)finishWriting {

    [self.fileWriteStream close];
    self.fileWriteStream = nil;

    [self fetchAttributes];
    if ( self.mimeType.length == 0 || [self.mimeType isEqualToString:@"application/octet-stream"]) {
        [self fetchMimeType];
    }
}

+ (NSURL *)temporaryFileURL {
    NSURL *directoryURL = [NSURL fileURLWithPath:[NSTemporaryDirectory() stringByAppendingPathComponent:[NSBundle mainBundle].bundleIdentifier] isDirectory:YES];

    if ( ![[NSFileManager defaultManager] fileExistsAtPath:directoryURL.path isDirectory:NULL] ) {
        NSError * error;
        if ( ! [[NSFileManager defaultManager] createDirectoryAtURL:directoryURL withIntermediateDirectories:YES attributes:nil error:&error] ) {
            [CRApp logErrorFormat:@"Unable to create temporary directory %@: %@", directoryURL, error];
            return nil;
        }
    }

    NSURL *fileURL =  [directoryURL URLByAppendingPathComponent:[NSProcessInfo processInfo].globallyUniqueString];
    return fileURL;
}

@end
