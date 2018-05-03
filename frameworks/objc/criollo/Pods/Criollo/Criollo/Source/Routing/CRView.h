//
//  CRView.h
//  Criollo
//
//  Created by Cătălin Stan on 5/17/14.
//  Copyright (c) 2014 Catalin Stan. All rights reserved.
//

NS_ASSUME_NONNULL_BEGIN

@interface CRView : NSObject 

@property (nonatomic, readonly, strong) NSString *contents;

- (instancetype)initWithContents:(NSString * _Nullable)contents NS_DESIGNATED_INITIALIZER;

- (NSString*)render:(NSDictionary * _Nullable)vars;

@end

NS_ASSUME_NONNULL_END
