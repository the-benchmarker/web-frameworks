//
//  CRRequestRange.h
//  Criollo
//
//  Created by Cătălin Stan on 06/03/16.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRTypes.h"

NS_ASSUME_NONNULL_BEGIN
@interface CRRequestByteRange : NSObject

@property (nonatomic, readonly, nullable) NSString *firstBytePos;
@property (nonatomic, readonly, nullable) NSString *lastBytePos;

- (NSRange)dataRangeForFileSize:(NSUInteger)fileSize;
- (BOOL)isSatisfiableForFileSize:(NSUInteger)fileSize;

- (NSString *)contentRangeSpecForFileSize:(NSUInteger)fileSize;
- (NSString *)contentLengthSpecForFileSize:(NSUInteger)fileSize;

@end

@interface CRRequestRange : NSObject

@property (nonatomic, readonly, strong) NSArray<CRRequestByteRange *> *byteRangeSet;
@property (nonatomic, readonly, strong) NSString *bytesUnit;

+ (instancetype)reuestRangeWithRangesSpecifier:(NSString *)rangesSpecifier;
+ (NSString *)acceptRangesSpec;

- (BOOL)isSatisfiableForFileSize:(NSUInteger)fileSize;

@end
NS_ASSUME_NONNULL_END
