//
//  CRHTTPServer.h
//  Criollo
//
//  Created by Cătălin Stan on 10/25/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//


#import "CRServer.h"

#define CRHTTPServerErrorDomain                     @"CRHTTPServerErrorDomain"
#define CRHTTPServerInternalError                   1000
#define CRHTTPServerInvalidCertificateBundle        1001
#define CRHTTPServerInvalidCertificatePrivateKey    1002

#define CRHTTPServerCertificatePathKey              @"CRHTTPServerCertificatePath"
#define CRHTTPServerCertificateKeyPathKey           @"CRHTTPServerCertificateKeyPath"

@class CRHTTPServerConfiguration;

@interface CRHTTPServer : CRServer

#if TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR
#else
@property (nonatomic) BOOL isSecure;

@property (nonatomic, strong, nullable) NSString *certificatePath;
@property (nonatomic, strong, nullable) NSString *certificateKeyPath;
#endif

@end
