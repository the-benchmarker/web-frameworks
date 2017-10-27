//
//  CRView.m
//  Criollo
//
//  Created by Cătălin Stan on 5/17/14.
//  Copyright (c) 2014 Catalin Stan. All rights reserved.
//

#import "CRView.h"

#define CRViewVariableFormat @"{{%@}}"

@interface CRView ()

@end

@implementation CRView

- (instancetype)init {
    return [self initWithContents:nil];
}

- (instancetype)initWithContents:(NSString *)contents {
    self = [super init];
    if ( self != nil ) {
        _contents = contents ? : @"";
    }
    return self;
}

- (NSString *)render:(NSDictionary<NSString*, NSString*> *)vars {
    if ( vars == nil ) {
        return self.contents;
    } else {
        NSMutableString* mutableContents = self.contents.mutableCopy;
        [vars enumerateKeysAndObjectsUsingBlock:^(NSString * _Nonnull key, NSString * _Nonnull obj, BOOL * _Nonnull stop) { @autoreleasepool {
            if ( ![obj isKindOfClass:[NSString class]] ) {
                return;
            }
            [mutableContents replaceOccurrencesOfString:[NSString stringWithFormat:CRViewVariableFormat, key] withString:obj options:0 range:NSMakeRange(0, mutableContents.length)];
        }}];
        return mutableContents;
    }
}

@end
