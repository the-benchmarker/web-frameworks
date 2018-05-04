//
//  CRRequestRange.m
//  Criollo
//
//  Created by Cătălin Stan on 06/03/16.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRRequestRange.h"
#import "CRRequestRange_Internal.h"
#import "CRRequest_Internal.h"

@implementation CRRequestByteRange

- (instancetype)init {
    return [self initWithByteRangeSpec:@""];
}

- (instancetype)initWithByteRangeSpec:(NSString *)byteRangeSpec {
    self = [super init];
    if ( self != nil ) {
        NSArray<NSString *>* byteRangeSpecComponents = [byteRangeSpec componentsSeparatedByString:@"-"];
        if( byteRangeSpecComponents.count == 2 ) {
            _firstBytePos = byteRangeSpecComponents[0];
            _lastBytePos = byteRangeSpecComponents[1];
        }
    }
    return self;
}

- (NSString *)description {
    NSMutableString* description = [super description].mutableCopy;
    [description appendFormat:@" firstBytePos: %@, lastBytePos: %@", _firstBytePos, _lastBytePos];
    return description;
}

// Parse a requested byte range (see: https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.1)
- (NSRange)dataRangeForFileSize:(NSUInteger)fileSize {
    NSRange dataRange = NSMakeRange(NSNotFound, 0);

    if ( self.firstBytePos.length > 0 ) {
        // byte-range
        dataRange.location = self.firstBytePos.integerValue;
        if ( self.lastBytePos.length > 0 ) {
            dataRange.length = self.lastBytePos.integerValue - self.firstBytePos.integerValue + 1;
        } else {
            dataRange.length = fileSize - dataRange.location;
        }
    } else {
        // suffix-range
        dataRange.length = self.lastBytePos.integerValue;
        if ( fileSize >= dataRange.length ) {
            dataRange.location = fileSize - dataRange.length;
        }
    }

    return dataRange;
}

- (BOOL)isSatisfiableForFileSize:(NSUInteger)fileSize {
    NSRange dataRange = [self dataRangeForFileSize:fileSize];
    return dataRange.length > 0 && dataRange.location != NSNotFound && dataRange.location + dataRange.length <= fileSize;
}

- (NSString *)contentRangeSpecForFileSize:(NSUInteger)fileSize {
    NSString* contentRangeSpec;
    NSString* fileSizeString = fileSize == UINT_MAX ? @"*" : @(fileSize).stringValue;
    if ([self isSatisfiableForFileSize:fileSize] ) {
        NSRange dataRange = [self dataRangeForFileSize:fileSize];
        contentRangeSpec = [NSString stringWithFormat:@"%lu-%lu/%@", (unsigned long)dataRange.location, (unsigned long)(dataRange.location + dataRange.length - 1), fileSizeString];
    } else {
        contentRangeSpec = [NSString stringWithFormat:@"*/%@", fileSizeString];
    }
    return contentRangeSpec;
}

- (NSString *)contentLengthSpecForFileSize:(NSUInteger)fileSize {
    NSString* contentLengthSpec;
    if ([self isSatisfiableForFileSize:fileSize] ) {
        NSRange dataRange = [self dataRangeForFileSize:fileSize];
        contentLengthSpec = @(dataRange.length).stringValue;
    } else {
        contentLengthSpec = @(fileSize).stringValue;
    }
    return contentLengthSpec;
}


@end

@implementation CRRequestRange

static const NSArray<NSString *> *acceptedRangeUnits;
static const NSString * acceptRangesSpec;

+ (void)initialize {
    acceptedRangeUnits = @[@"bytes"];
    acceptRangesSpec = [[CRRequestRange acceptedRangeUnits] componentsJoinedByString:CRRequestHeaderSeparator];
    if ( acceptRangesSpec.length == 0 ) {
        acceptRangesSpec = @"none";
    }
}

+ (NSArray<NSString *> *)acceptedRangeUnits {
    return (NSArray<NSString *> *)acceptedRangeUnits;
}

+ (NSString *)acceptRangesSpec {
    return (NSString *)acceptRangesSpec;
}

+ (instancetype)reuestRangeWithRangesSpecifier:(NSString *)rangesSpecifier {
    CRRequestRange* requestRange = [[CRRequestRange alloc] initWithRangesSpecifier:rangesSpecifier];
    return requestRange;
}

- (instancetype)init {
    return [self initWithRangesSpecifier:@""];
}

- (instancetype)initWithRangesSpecifier:(NSString *)rangesSpecifier {
    self = [super init];
    if ( self != nil ) {
        if ( rangesSpecifier.length > 0 ) {
            NSString* byteRangesSpecifier = [rangesSpecifier componentsSeparatedByString:CRRequestHeaderSeparator][0];
            NSArray* byteRangesSpecifierComponents = [byteRangesSpecifier componentsSeparatedByString:@"="];
            if ( byteRangesSpecifierComponents.count == 2 ) {
                _bytesUnit = byteRangesSpecifierComponents[0];
                NSString* byteRangeSetSepecifier = byteRangesSpecifierComponents[1];
                if ( byteRangeSetSepecifier.length > 0 ) {
                    NSArray<NSString *>* byteRangeSpecs = [byteRangeSetSepecifier componentsSeparatedByString:CRRequestHeaderArraySeparator];
                    NSMutableArray<CRRequestByteRange *> *byteRangeSet = [NSMutableArray arrayWithCapacity:byteRangeSpecs.count];
                    [byteRangeSpecs enumerateObjectsUsingBlock:^(NSString * _Nonnull byteRangeSpec, NSUInteger idx, BOOL * _Nonnull stop) {
                        CRRequestByteRange *byteRange = [[CRRequestByteRange alloc] initWithByteRangeSpec:byteRangeSpec];
                        [byteRangeSet addObject:byteRange];
                    }];
                    _byteRangeSet = byteRangeSet;
                }
            }
        }
    }
    return self;
}

- (NSString *)description {
    NSMutableString* description = [super description].mutableCopy;
    [description appendFormat:@" bytesUnit: %@, byteRangeSet: %@", _bytesUnit, _byteRangeSet];
    return description;
}

- (BOOL)isSatisfiableForFileSize:(NSUInteger)fileSize {
    __block BOOL isSatisfiable = [[CRRequestRange acceptedRangeUnits] containsObject:self.bytesUnit] && self.byteRangeSet.count > 0;
    [self.byteRangeSet enumerateObjectsUsingBlock:^(CRRequestByteRange * _Nonnull byteRange, NSUInteger idx, BOOL * _Nonnull stop) {
        if ( ![byteRange isSatisfiableForFileSize:fileSize] ) {
            isSatisfiable = NO;
            *stop = YES;
        }
    }];
    return isSatisfiable;
}

@end
