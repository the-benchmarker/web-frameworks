//
//  CRFCGIServerConfiguration.h
//  Criollo
//
//  Created by Cătălin Stan on 10/30/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRServerConfiguration.h"

@interface CRFCGIServerConfiguration : CRServerConfiguration

@property (nonatomic, assign) NSUInteger CRFCGIConnectionReadRecordTimeout;
@property (nonatomic, assign) NSUInteger CRFCGIConnectionSocketWriteBuffer;

@end
