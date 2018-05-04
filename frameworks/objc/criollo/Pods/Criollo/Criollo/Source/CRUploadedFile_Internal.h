//
//  CRUploadedFile_Internal.h
//  Criollo
//
//  Created by Cătălin Stan on 19/10/2016.
//  Copyright © 2016 Cătălin Stan. All rights reserved.
//

#import "CRUploadedFile.h"

NS_ASSUME_NONNULL_BEGIN

@interface CRUploadedFile ()

- (instancetype)initWithName:(NSString *)name NS_DESIGNATED_INITIALIZER;

- (void)fetchAttributes;
- (void)fetchMimeType;

- (void)appendData:(NSData *)data;
- (void)finishWriting;

@end

NS_ASSUME_NONNULL_END
