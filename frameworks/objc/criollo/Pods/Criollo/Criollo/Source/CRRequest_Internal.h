//
//  CRRequest_Internal.h
//  Criollo
//
//  Created by Cătălin Stan on 11/20/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRRequest.h"

#define CRRequestHeaderNameSeparator        @":"
#define CRRequestHeaderSeparator            @";"
#define CRRequestHeaderArraySeparator       @","
#define CRRequestKeySeparator               @"&"
#define CRRequestValueSeparator             @"="
#define CRRequestBoundaryParameter          @"boundary"
#define CRRequestBoundaryPrefix             @"--"

NS_ASSUME_NONNULL_BEGIN

@interface CRRequest ()

@property (nonatomic, readonly) BOOL shouldCloseConnection;

@property (nonatomic, strong, nullable) NSMutableData * bufferedBodyData;
@property (nonatomic, strong, nullable) NSMutableData * bufferedResponseData;

@property (nonatomic, readonly, nullable) NSString * multipartBoundary;
@property (nonatomic, readonly) NSData * multipartBoundaryPrefixData;
@property (nonatomic, readonly, nullable) NSString * multipartBoundaryPrefixedString;
@property (nonatomic, readonly, nullable) NSData * multipartBoundaryPrefixedData;

- (instancetype)initWithMethod:(CRHTTPMethod)method URL:(NSURL * _Nullable)URL version:(CRHTTPVersion)version;
- (instancetype)initWithMethod:(CRHTTPMethod)method URL:(NSURL * _Nullable)URL version:(CRHTTPVersion)version connection:(CRConnection* _Nullable) connection;
- (instancetype)initWithMethod:(CRHTTPMethod)method URL:(NSURL * _Nullable)URL version:(CRHTTPVersion)version connection:(CRConnection* _Nullable) connection env:(NSDictionary* _Nullable)env NS_DESIGNATED_INITIALIZER;

- (BOOL)appendData:(NSData *)data;
- (void)bufferBodyData:(NSData *)data;
- (void)bufferResponseData:(NSData *)data;

- (void)clearBodyParsingTargets;

- (BOOL)appendBodyData:(NSData *)data forKey:(NSString *)key;
- (BOOL)setFileHeader:(NSDictionary *)headerFields forKey:(NSString *)key;
- (BOOL)appendFileData:(NSData *)data forKey:(NSString *)key;

- (void)setEnv:(NSDictionary<NSString *,NSString *> *)envDictionary;
- (void)setEnv:(NSString *)obj forKey:(NSString *)key;

- (void)setQuery:(NSString *)obj forKey:(NSString *)key;

- (BOOL)parseJSONBodyData:(NSError * _Nullable __autoreleasing * _Nullable)error;
- (BOOL)parseMIMEBodyDataChunk:(NSData *)data error:(NSError *__autoreleasing  _Nullable * _Nullable)error;
- (BOOL)parseMultipartBodyDataChunk:(NSData *)data error:(NSError * _Nullable __autoreleasing * _Nullable)error;
- (BOOL)parseURLEncodedBodyData:(NSError * _Nullable __autoreleasing * _Nullable)error;
- (BOOL)parseBufferedBodyData:(NSError * _Nullable __autoreleasing * _Nullable)error;

@end

NS_ASSUME_NONNULL_END
