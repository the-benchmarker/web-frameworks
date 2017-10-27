//
//  CRRequest.m
//  Criollo
//
//  Created by Cătălin Stan on 3/30/14.
//  Copyright (c) 2014 Catalin Stan. All rights reserved.
//

#import "CRMessage_Internal.h"
#import "CRRequest.h"
#import "CRRequest_Internal.h"
#import "CRConnection.h"
#import "CRConnection_Internal.h"
#import "CRServer.h"
#import "CRServer_Internal.h"
#import "CRRequestRange.h"
#import "CRRequestRange_Internal.h"
#import "CRUploadedFile.h"
#import "CRUploadedFile_Internal.h"
#import "CRApplication.h"

#import "NSString+Criollo.h"

#define CRFileHeaderNameKey             @"name"
#define CRFileHeaderFilenameKey         @"filename"
#define CRFileHeaderContentTypeKey      @"content-type"

@implementation CRRequest {
    __strong NSMutableDictionary* _env;

    __block NSString * _multipartBoundary;
    __block dispatch_once_t _multipartBoundaryOnceToken;

    __block NSString * _multipartBoundaryPrefixedString;
    __block dispatch_once_t _multipartBoundaryPrefixedStringOnceToken;

    __block NSData * _multipartBoundaryPrefixedData;
    __block dispatch_once_t _multipartBoundaryPrefixedDataOnceToken;

    NSString* currentMultipartBodyKey;
    NSString* currentMultipartFileKey;
}

- (instancetype)init {
    return [self initWithMethod:CRHTTPMethodGet URL:nil version:CRHTTPVersion1_1 connection:nil env:nil];
}

- (instancetype)initWithMethod:(CRHTTPMethod)method URL:(NSURL *)URL version:(CRHTTPVersion)version {
    return [self initWithMethod:method URL:URL version:version connection:nil env:nil];
}

- (instancetype)initWithMethod:(CRHTTPMethod)method URL:(NSURL *)URL version:(CRHTTPVersion)version connection:(CRConnection * _Nullable)connection {
    return [self initWithMethod:method URL:URL version:version connection:connection env:nil];
}
- (instancetype)initWithMethod:(CRHTTPMethod)method URL:(NSURL *)URL version:(CRHTTPVersion)version connection:(CRConnection *)connection env:(NSDictionary *)env {
    self = [super init];
    if ( self != nil ) {
        _method = method;
        if ( connection ) {
            _connection = connection;
        }
        if ( URL ) {
            self.message = CFBridgingRelease( CFHTTPMessageCreateRequest(NULL, (__bridge CFStringRef)NSStringFromCRHTTPMethod(_method), (__bridge CFURLRef)URL, (__bridge CFStringRef)NSStringFromCRHTTPVersion(version)) );
        }
        if ( env == nil ) {
            _env = [NSMutableDictionary dictionary];
        } else {
            [self setEnv:env];
        }
        _query = [NSMutableDictionary dictionary];
    }
    return self;
}

- (NSURL *)URL {
    return (__bridge_transfer NSURL *)CFHTTPMessageCopyRequestURL((__bridge CFHTTPMessageRef)self.message);
}

- (BOOL)appendData:(NSData *)data {
    return CFHTTPMessageAppendBytes((__bridge CFHTTPMessageRef)self.message, data.bytes, data.length);
}

- (NSDictionary<NSString *,NSString *> *)env {
    return _env;
}

- (void)setEnv:(NSDictionary<NSString *,NSString *> *)envDictionary {
    if ( [envDictionary isKindOfClass:[NSMutableDictionary class]] ) {
        _env = (NSMutableDictionary*)envDictionary;
    } else {
        _env = envDictionary.mutableCopy;
    }

    // Parse request query string
    NSMutableDictionary<NSString *,NSString *> *query = _query ? _query.mutableCopy : [NSMutableDictionary dictionary];
    if ( _env[@"QUERY_STRING"] != nil ) {
        NSArray<NSString *> *queryVars = [_env[@"QUERY_STRING"] componentsSeparatedByString:CRRequestKeySeparator];
        [queryVars enumerateObjectsUsingBlock:^(NSString*  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) { @autoreleasepool {
            NSArray<NSString *> *queryVarComponents = [[obj stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] componentsSeparatedByString:CRRequestValueSeparator];
            NSString *key = queryVarComponents[0].stringByDecodingURLEncodedString.stringByRemovingPercentEncoding ? : (queryVarComponents[0].stringByDecodingURLEncodedString ? : queryVarComponents[0]);
            NSString *value = queryVarComponents.count > 1 ? (queryVarComponents[1].stringByDecodingURLEncodedString.stringByRemovingPercentEncoding ? : (queryVarComponents[0].stringByDecodingURLEncodedString ? : queryVarComponents[1])) : @"";
            query[key] = value;
        }}];
    }
    _query = query;

    // Parse request cookies
    NSMutableDictionary<NSString *,NSString *> *cookies = [NSMutableDictionary dictionary];
    if ( _env[@"HTTP_COOKIE"] != nil ) {
        NSArray<NSString *> *cookieStrings = [_env[@"HTTP_COOKIE"] componentsSeparatedByString:CRRequestHeaderSeparator];
        [cookieStrings enumerateObjectsUsingBlock:^(NSString*  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) { @autoreleasepool {
            NSArray<NSString *> *cookieComponents = [[obj stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] componentsSeparatedByString:CRRequestValueSeparator];
            cookies[cookieComponents[0]] = cookieComponents.count > 1 ? cookieComponents[1] : @"";
        }}];
    }
    _cookies = cookies;

    // Parse Range header
    if ( _env[@"HTTP_RANGE"] != nil ) {
        _range = [CRRequestRange reuestRangeWithRangesSpecifier:_env[@"HTTP_RANGE"]];
    }
}

- (void)setEnv:(NSString *)obj forKey:(NSString *)key {
    [_env setObject:obj forKey:key];
}

- (void)setQuery:(NSString *)obj forKey:(NSString *)key {
    [((NSMutableDictionary *)_query) setObject:obj forKey:key];
}

- (NSString *)description {
    return [NSString stringWithFormat:@"%@ %@ %@", NSStringFromCRHTTPMethod(self.method), self.URL.path, NSStringFromCRHTTPVersion(self.version)];
}

- (BOOL)shouldCloseConnection {
    BOOL shouldClose = NO;

    NSString *connectionHeader = [self valueForHTTPHeaderField:@"Connection"];
    if ( connectionHeader != nil ) {
        shouldClose = [connectionHeader caseInsensitiveCompare:@"close"] == NSOrderedSame;
    } else {
        shouldClose = self.version == CRHTTPVersion1_0;
    }

    return shouldClose;
}

#pragma mark - Body parsing

- (void)clearBodyParsingTargets {
    currentMultipartBodyKey = nil;
    currentMultipartFileKey = nil;
}

- (BOOL)parseJSONBodyData:(NSError *__autoreleasing  _Nullable *)error {
    BOOL result = NO;

    NSError* jsonDecodingError;
    id decodedBody = [NSJSONSerialization JSONObjectWithData:self.bufferedBodyData options:0 error:&jsonDecodingError];

    if ( jsonDecodingError == nil ) {
        _body = decodedBody;
        result = YES;
        self.bufferedBodyData = nil;
    } else {
        *error = [NSError errorWithDomain:CRRequestErrorDomain code:CRRequestErrorMalformedBody userInfo:@{NSLocalizedDescriptionKey:@"Unable to parse JSON request.", NSUnderlyingErrorKey:jsonDecodingError}];
    }

    return result;
}

- (BOOL)parseMIMEBodyDataChunk:(NSData *)data error:(NSError *__autoreleasing  _Nullable * _Nullable)error {
    BOOL result = YES;

    if ( currentMultipartFileKey == nil ) {
        // Set the target for the value
        currentMultipartFileKey = @(self.files.count).stringValue;
        NSString *contentType = self.env[@"HTTP_CONTENT_TYPE"];
        NSRange separatorRange = [contentType rangeOfString:CRRequestHeaderSeparator];
        if ( separatorRange.location != NSNotFound ) {
            contentType = [contentType substringToIndex:separatorRange.location];
        }
        if ( contentType.length == 0 ) {
            contentType = @"application/octet-stream";
        }
        NSDictionary *headerFields = @{ CRFileHeaderNameKey: @"", CRFileHeaderFilenameKey: @"", CRFileHeaderContentTypeKey: contentType };
        [self setFileHeader:headerFields forKey:currentMultipartFileKey];
    }

    if ( currentMultipartFileKey != nil ) {
        result = [self appendFileData:data forKey:currentMultipartFileKey];
    } else {
        result = NO;
        *error = [NSError errorWithDomain:CRRequestErrorDomain code:CRRequestFileWriteError userInfo:@{NSLocalizedDescriptionKey: NSLocalizedString(@"Unable to append MIME data",)}];
    }
        
    return result;
}

- (BOOL)parseMultipartBodyDataChunk:(NSData *)data error:(NSError *__autoreleasing  _Nullable * _Nullable)error {
    BOOL result = YES;

    // Search for a boundary
    NSRange searchRange = NSMakeRange(0, data.length);
    NSRange nextBoundaryRange = [data rangeOfData:self.multipartBoundaryPrefixedData options:0 range:searchRange];

    if ( nextBoundaryRange.location != NSNotFound ) {                                   // We have a boundary

        NSData* CRLFData = [CRConnection CRLFData];
        NSData* CRLFCRLFData = [CRConnection CRLFCRLFData];

        // Check if we have something before the boundary
        if ( nextBoundaryRange.location != 0 ) {                                        // There is an existing chunk

            // Extract the piece
            NSData* preambleData = [NSData dataWithBytesNoCopy:(void *)data.bytes length:nextBoundaryRange.location - CRLFData.length freeWhenDone:NO];

            // Check if we have something buffered
            if ( self.bufferedBodyData.length > 0 ) {                                   // This is part of a field that

                // Prepend the the buffered data to the one piece
                NSMutableData* bufferedAndPreambleData = [NSMutableData dataWithCapacity:(self.bufferedBodyData.length + nextBoundaryRange.location)];
                [bufferedAndPreambleData appendData:self.bufferedBodyData];
                [bufferedAndPreambleData appendData:preambleData];

                // Flush the buffered data
                self.bufferedBodyData = nil;

                // Call this method again with the combined data
                result = [self parseMultipartBodyDataChunk:bufferedAndPreambleData error:error];

            } else {

                // Append the piece to the target if there is one otherwise discard it
                // (RFC 1341 says to discard anything before the first --boundary (the preamble)
                // http://www.w3.org/Protocols/rfc1341/7_2_Multipart.html

                if (currentMultipartBodyKey != nil) {
                    result = [self appendBodyData:preambleData forKey:currentMultipartBodyKey];
                } else if ( currentMultipartFileKey != nil) {
                    result = [self appendFileData:preambleData forKey:currentMultipartFileKey];
                }
            }

            if ( result ) {
                // Extract the remaining data
                NSData* nextChunkData = [NSData dataWithBytesNoCopy:(void *)data.bytes + nextBoundaryRange.location length:data.length - nextBoundaryRange.location freeWhenDone:NO];

                // Call this method again with the remaining data
                result = [self parseMultipartBodyDataChunk:nextChunkData error:error];
            }

        } else {                                                                        // We are starting with a new field

            // Read the header (starts after the --boundary and a CRLF and ends with CRLFCRLF)
            NSUInteger headerStartLocation = nextBoundaryRange.location + nextBoundaryRange.length + CRLFData.length;
            NSRange headerSearchRange = NSMakeRange(headerStartLocation, data.length - headerStartLocation);
            NSRange headerTerminatorRange = [data rangeOfData:CRLFCRLFData options:0 range:headerSearchRange];

            if ( headerTerminatorRange.location != NSNotFound ) {                                 // We have a header - all good

                NSData* headerData = [NSData dataWithBytesNoCopy:(void *)data.bytes + headerSearchRange.location length:headerTerminatorRange.location - headerSearchRange.location freeWhenDone:NO];
                NSString* headerString = [[NSString alloc] initWithBytesNoCopy:(void *)headerData.bytes length:headerData.length encoding:NSUTF8StringEncoding freeWhenDone:NO];

                // Parse the header
                NSMutableDictionary<NSString *, NSString *> * headerFields = [NSMutableDictionary dictionary];
                NSArray<NSString *> * headerLines = [headerString componentsSeparatedByString:@"\r\n"];
                [headerLines enumerateObjectsUsingBlock:^(NSString * _Nonnull headerLine, NSUInteger idx, BOOL * _Nonnull stop) { @autoreleasepool {
                    NSArray* headerComponents = [headerLine componentsSeparatedByString:CRRequestHeaderNameSeparator];
                    NSString* headerName = [headerComponents[0] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]].lowercaseString;
                    NSString* headerValue = [headerComponents[1] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
                    NSArray* headerValueComponents = [headerValue componentsSeparatedByString:CRRequestHeaderSeparator];
                    [headerValueComponents enumerateObjectsUsingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) { @autoreleasepool {
                        if ( idx == 0 ) {
                            headerFields[headerName] = [obj stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
                            return;
                        }

                        NSArray* headerValueComponentsParts = [obj componentsSeparatedByString:CRRequestValueSeparator];
                        NSString* headerValueComponentsPartName = [headerValueComponentsParts[0] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
                        NSString* headerValueComponentsPartValue = [headerValueComponentsParts[1] stringByTrimmingCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:@"\" "]];
                        headerFields[headerValueComponentsPartName] = headerValueComponentsPartValue;
                    }}];
                }}];

                // Close any file output streams
                if ( currentMultipartFileKey != nil ) {
                    [self.files[currentMultipartFileKey] finishWriting];
                }

                // Set the target for the value
                [self clearBodyParsingTargets];
                if ( headerFields[CRFileHeaderNameKey].length > 0 ) {
                    if ( headerFields[CRFileHeaderFilenameKey] && headerFields[CRFileHeaderFilenameKey].length > 0 ) {
                        currentMultipartFileKey = headerFields[CRFileHeaderNameKey];
                    } else if ( !headerFields[CRFileHeaderFilenameKey] ) {
                        currentMultipartBodyKey = headerFields[CRFileHeaderNameKey];
                    }
                }

                // Create the file
                if ( currentMultipartFileKey != nil ) {
                    [self setFileHeader:headerFields forKey:currentMultipartFileKey];
                }

                // Extract the remaining data
                NSData* nextChunkData = [NSData dataWithBytesNoCopy:(void *)data.bytes + headerTerminatorRange.location + headerTerminatorRange.length length:data.length - headerTerminatorRange.location - headerTerminatorRange.length freeWhenDone:NO];

                // Call this method again with the remaining data
                result = [self parseMultipartBodyDataChunk:nextChunkData error:error];

            } else {
                // Checkk if this is the end of the message "--boundary--"
                NSRange terminatorSearchRange = NSMakeRange(nextBoundaryRange.location + nextBoundaryRange.length, data.length - nextBoundaryRange.location - nextBoundaryRange.length);
                NSRange terminatorCRLFRange = [data rangeOfData:CRLFData options:0 range:terminatorSearchRange];

                NSData * terminatorData = [NSData dataWithBytesNoCopy:(void *)data.bytes + terminatorSearchRange.location length:data.length - terminatorCRLFRange.location freeWhenDone:NO];
                if ( [terminatorData isEqualToData:self.multipartBoundaryPrefixData] ) {
                    // This is the end of the message
                    result = YES;
                } else {
                    // There is no header something is very wrong
                    if ( *error != nil ) {
                        *error = [NSError errorWithDomain:CRRequestErrorDomain code:CRRequestErrorMalformedBody userInfo:@{NSLocalizedDescriptionKey: NSLocalizedString(@"Unable to parse multipart data.",)}];
                    }
                    result = NO;
                }
            }

        }

    } else {                                                                            // This is just a chunk of something

        // Append the data to the target if there is one otherwise discard it
        // (RFC 1341 says to discard anything before the first --boundary (the preamble)
        // http://www.w3.org/Protocols/rfc1341/7_2_Multipart.html

        if ( currentMultipartBodyKey != nil ) {
            result = [self appendBodyData:data forKey:currentMultipartBodyKey];
        } else if ( currentMultipartFileKey != nil ) {
            result = [self appendFileData:data forKey:currentMultipartFileKey];
        }

    }
    return result;
}

- (void)bufferBodyData:(NSData*)data {
    if ( self.bufferedBodyData == nil ) {
        self.bufferedBodyData = [NSMutableData dataWithData:data];
    } else {
        [self.bufferedBodyData appendData:data];
    }
}

- (BOOL)appendBodyData:(NSData *)data forKey:(NSString *)key {
    if ( _body == nil ) {
        _body = [NSMutableDictionary dictionary];
    }

    NSString* dataString = [[NSString alloc] initWithBytesNoCopy:(void *)data.bytes length:data.length encoding:NSUTF8StringEncoding freeWhenDone:NO];
    if ( self.body[key] == nil ) {
        self.body[key] = [NSMutableString stringWithString:dataString];
    } else {
        [((NSMutableString *) self.body[key]) appendString:dataString];
    }

    return YES;
}

- (BOOL)setFileHeader:(NSDictionary *)headerFields forKey:(NSString *)key {
    if ( self.files == nil ) {
        _files = [NSMutableDictionary dictionary];
    }

    CRUploadedFile * file = [[CRUploadedFile alloc] initWithName:headerFields[CRFileHeaderFilenameKey]];
    file.mimeType = headerFields[CRFileHeaderContentTypeKey];

    ((NSMutableDictionary *)self.files)[key] = file;

    return YES;
}

- (BOOL)appendFileData:(NSData *)data forKey:(NSString *)key {
    if ( self.files[key] == nil ) {
        return NO;
    }

    [self.files[key] appendData:data];
    return YES;
}

- (void)bufferResponseData:(NSData *)data {
    if ( self.bufferedResponseData == nil ) {
        self.bufferedResponseData = [NSMutableData dataWithData:data];
    } else {
        [self.bufferedResponseData appendData:data];
    }
}

- (BOOL)parseURLEncodedBodyData:(NSError *__autoreleasing  _Nullable *)error {
    NSMutableDictionary<NSString *,NSString *> *body = [NSMutableDictionary dictionary];

    NSString* bodyString = [[NSString alloc] initWithBytesNoCopy:(void *)self.bufferedBodyData.bytes length:self.bufferedBodyData.length encoding:NSUTF8StringEncoding freeWhenDone:NO];
    NSArray<NSString *> *bodyVars = [bodyString componentsSeparatedByString:CRRequestKeySeparator];
    [bodyVars enumerateObjectsUsingBlock:^(NSString*  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) { @autoreleasepool {
        NSArray<NSString *> *bodyVarComponents = [[obj stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] componentsSeparatedByString:CRRequestValueSeparator];
        NSString *key = bodyVarComponents[0].stringByDecodingURLEncodedString.stringByRemovingPercentEncoding ? : (bodyVarComponents[0].stringByDecodingURLEncodedString ? : bodyVarComponents[0]);
        NSString *value = bodyVarComponents.count > 1 ? (bodyVarComponents[1].stringByDecodingURLEncodedString.stringByRemovingPercentEncoding ? : (bodyVarComponents[0].stringByDecodingURLEncodedString ? : bodyVarComponents[1])) : @"";
        body[key] = value;
    }}];
    _body = body;
    self.bufferedBodyData = nil;
    return YES;
}

- (BOOL)parseBufferedBodyData:(NSError *__autoreleasing  _Nullable *)error {
    _body = [NSData dataWithBytesNoCopy:(void *)self.bufferedBodyData.bytes length:self.bufferedBodyData.length freeWhenDone:NO];
    return YES;
}

- (NSString *)multipartBoundary {
    NSString* contentType = _env[@"HTTP_CONTENT_TYPE"];
    if ([contentType hasPrefix:CRRequestTypeMultipart]) {
        dispatch_once(&_multipartBoundaryOnceToken, ^{
            NSArray<NSString*>* headerComponents = [contentType componentsSeparatedByString:CRRequestHeaderSeparator];
            [headerComponents enumerateObjectsUsingBlock:^(NSString * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                obj = [obj stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
                if ( ![obj hasPrefix:CRRequestBoundaryParameter] ) {
                    return;
                }
                _multipartBoundary = [[obj componentsSeparatedByString:CRRequestValueSeparator][1] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
            }];
        });
    }
    return _multipartBoundary;
}

- (NSData *)multipartBoundaryPrefixData {
    static NSData * _multipartBoundaryPrefixData;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        _multipartBoundaryPrefixData = [NSData dataWithBytesNoCopy:(void * )CRRequestBoundaryPrefix.UTF8String length:CRRequestBoundaryPrefix.length freeWhenDone:NO];
    });
    return _multipartBoundaryPrefixData;
}

- (NSString *)multipartBoundaryPrefixedString {
    if ( self.multipartBoundary.length > 0 ) {
        dispatch_once(&_multipartBoundaryPrefixedStringOnceToken, ^{
            _multipartBoundaryPrefixedString = [NSString stringWithFormat:@"%@%@", CRRequestBoundaryPrefix, self.multipartBoundary];
        });
    }
    return _multipartBoundaryPrefixedString;
}

- (NSData *)multipartBoundaryPrefixedData {
    if ( self.multipartBoundaryPrefixedString.length > 0 ) {
        dispatch_once(&_multipartBoundaryPrefixedDataOnceToken, ^{
            _multipartBoundaryPrefixedData = [NSData dataWithBytesNoCopy:(void *)self.multipartBoundaryPrefixedString.UTF8String length:self.multipartBoundaryPrefixedString.length freeWhenDone:NO];
        });
    }
    return _multipartBoundaryPrefixedData;
}

- (void)dealloc {
    // Delete temporary files created by multipart uploadds
    if ( self.files.count != 0 ) {
        [self.files enumerateKeysAndObjectsWithOptions:NSEnumerationConcurrent usingBlock:^(NSString * _Nonnull key, CRUploadedFile * _Nonnull obj, BOOL * _Nonnull stop) {
            NSURL * temporaryFileURL = obj.temporaryFileURL;
            dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0), ^{
                [[NSFileManager defaultManager] removeItemAtURL:temporaryFileURL error:nil];
            });
        }];
    }

}

@end
