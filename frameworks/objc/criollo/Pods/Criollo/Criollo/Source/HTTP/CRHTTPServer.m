//
//  CRHTTPServer.m
//  Criollo
//
//  Created by Cătălin Stan on 10/25/15.
//  Copyright © 2015 Cătălin Stan. All rights reserved.
//

#import "CRServer_Internal.h"
#import "CRHTTPServer.h"
#import "CRHTTPConnection.h"
#import "CRConnection_Internal.h"
#import "CRHTTPServerConfiguration.h"
#import "GCDAsyncSocket.h"
#import "CRApplication.h"

@interface CRHTTPServer () <GCDAsyncSocketDelegate>

#if TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR
#else

@property (nonatomic, strong, nullable, readonly) NSArray *certificates;

- (nullable NSArray *)fetchIdentityAndCertificatesWithError:(NSError * _Nullable __autoreleasing * _Nullable)error;

#endif

@end

@implementation CRHTTPServer {
#if TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR
#else
    NSArray * _certificates;
#endif
}

- (instancetype)initWithDelegate:(id<CRServerDelegate>)delegate delegateQueue:(dispatch_queue_t)delegateQueue {
    self = [super initWithDelegate:delegate delegateQueue:delegateQueue];
    if ( self != nil ) {
        self.configuration = [[CRHTTPServerConfiguration alloc] init];
    }
    return self;
}

- (CRConnection*)newConnectionWithSocket:(GCDAsyncSocket*)socket {
    CRHTTPConnection* connection = [[CRHTTPConnection alloc] initWithSocket:socket server:self];
#if TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR
#else
    if ( self.isSecure && self.certificates.count > 0 ) {
        NSMutableDictionary *settings = [NSMutableDictionary dictionaryWithCapacity:3];
        settings[(__bridge NSString *)kCFStreamSSLIsServer] = @YES;
        settings[(__bridge NSString *)kCFStreamSSLCertificates] = self.certificates;
        settings[(__bridge NSString *)kCFStreamPropertySocketSecurityLevel] = (__bridge NSString *)(kCFStreamSocketSecurityLevelNegotiatedSSL);
        [connection.socket startTLS:settings];
    }
#endif
    return connection;
}

- (BOOL)startListening:(NSError *__autoreleasing  _Nullable *)error portNumber:(NSUInteger)portNumber interface:(NSString *)interface {
#if TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR
#else
    NSError * certificateParsingError;
    _certificates = [self fetchIdentityAndCertificatesWithError:&certificateParsingError];
    if ( _certificates == nil ) {
        [CRApp logErrorFormat:NSLocalizedString(@"Unable to parse certificates: %@",), certificateParsingError];
        return NO;
    }
#endif
    return [super startListening:error portNumber:portNumber interface:interface];
}

#if TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR
#else
- (NSArray *)fetchIdentityAndCertificatesWithError:(NSError *__autoreleasing  _Nullable *)error {

    if ( self.certificatePath.length == 0 || self.certificateKeyPath.length == 0 ) {
        return [NSArray array];
    }

    // Read the contents of the PEM chained bundle and get SecCertRefs
    NSError * pemCertReadError;
    NSData * pemCertContents = [NSData dataWithContentsOfFile:self.certificatePath options:NSDataReadingUncached error:&pemCertReadError];
    if ( pemCertContents.length == 0 ) {
        *error = [[NSError alloc] initWithDomain:CRHTTPServerErrorDomain code:CRHTTPServerInvalidCertificateBundle userInfo:@{NSLocalizedDescriptionKey: NSLocalizedString(@"Unable to parse PEM certificate bundle.",), NSUnderlyingErrorKey: pemCertReadError, CRHTTPServerCertificatePathKey: self.certificatePath ? : @"(null)", CRHTTPServerCertificateKeyPathKey: self.certificateKeyPath ? : @"(null)"}];
        return nil;
    }

    CFArrayRef certificateImportItems = NULL;
    OSStatus certificateImportStatus = SecItemImport((__bridge CFDataRef)pemCertContents , NULL, NULL, NULL, 0, NULL, NULL, &certificateImportItems);
    if ( certificateImportStatus != errSecSuccess ) {
        *error = [[NSError alloc] initWithDomain:CRHTTPServerErrorDomain code:CRHTTPServerInvalidCertificateBundle userInfo:@{NSLocalizedDescriptionKey:[NSString stringWithFormat: NSLocalizedString(@"Unable to parse PEM certificate bundle. %@",), (__bridge NSString *)SecCopyErrorMessageString(certificateImportStatus, NULL)], CRHTTPServerCertificatePathKey: self.certificatePath ? : @"(null)", CRHTTPServerCertificateKeyPathKey: self.certificateKeyPath ? : @"(null)"}];
        return nil;
    }

    // Check if the first item in the import is a certificate
    SecCertificateRef certificate = (SecCertificateRef) CFArrayGetValueAtIndex(certificateImportItems, 0);
    if ( CFGetTypeID(certificate) != SecCertificateGetTypeID() ) {
        *error = [[NSError alloc] initWithDomain:CRHTTPServerErrorDomain code:CRHTTPServerInvalidCertificateBundle userInfo:@{NSLocalizedDescriptionKey:[NSString stringWithFormat: NSLocalizedString(@"Expected a PEM certificate, but got %@ instead.",), (__bridge id)certificate], CRHTTPServerCertificatePathKey: self.certificatePath ? : @"(null)", CRHTTPServerCertificateKeyPathKey: self.certificateKeyPath ? : @"(null)"}];
        return nil;
    }

    // Read the contents of the PEM private key file and fetch SecKeyRef
    NSError * pemKeyReadError;
    NSData * pemKeyContents = [NSData dataWithContentsOfFile:self.certificateKeyPath options:NSDataReadingUncached error:&pemKeyReadError];
    if ( pemKeyContents.length == 0 ) {
        *error = [[NSError alloc] initWithDomain:CRHTTPServerErrorDomain code:CRHTTPServerInvalidCertificatePrivateKey userInfo:@{NSLocalizedDescriptionKey: NSLocalizedString(@"Unable to parse PEM RSA key file.",), NSUnderlyingErrorKey: pemKeyReadError, CRHTTPServerCertificatePathKey: self.certificatePath ? : @"(null)", CRHTTPServerCertificateKeyPathKey: self.certificateKeyPath ? : @"(null)"}];
        return nil;
    }

    // Create a temp keychain and import the private key into it
    NSString *keychainPath = [NSTemporaryDirectory() stringByAppendingPathComponent:[NSUUID UUID].UUIDString];
    NSString *keychainPassword = [NSUUID UUID].UUIDString;
    SecKeychainRef keychain;
    OSStatus keychainCreationStatus = SecKeychainCreate(keychainPath.UTF8String, (UInt32)keychainPassword.length, keychainPassword.UTF8String, NO, NULL, &keychain);
    if ( keychainCreationStatus != errSecSuccess ) {
        *error = [[NSError alloc] initWithDomain:CRHTTPServerErrorDomain code:CRHTTPServerInternalError userInfo:@{NSLocalizedDescriptionKey:[NSString stringWithFormat: NSLocalizedString(@"Unable to create keychain. %@",), (__bridge NSString *)SecCopyErrorMessageString(keychainCreationStatus, NULL)], CRHTTPServerCertificatePathKey: self.certificatePath ? : @"(null)", CRHTTPServerCertificateKeyPathKey: self.certificateKeyPath ? : @"(null)"}];
        return nil;
    }

    // Import the key into the keychain
    CFArrayRef keyImportItems = NULL;
    OSStatus keyImportStatus = SecItemImport((__bridge CFDataRef)pemKeyContents , NULL, NULL, NULL, 0, NULL, keychain, &keyImportItems);
    if ( keyImportStatus != errSecSuccess ) {
        *error = [[NSError alloc] initWithDomain:CRHTTPServerErrorDomain code:CRHTTPServerInvalidCertificatePrivateKey userInfo:@{NSLocalizedDescriptionKey:[NSString stringWithFormat: NSLocalizedString(@"Unable to parse PEM RSA key file. %@",), (__bridge NSString *)SecCopyErrorMessageString(keyImportStatus, NULL)], CRHTTPServerCertificatePathKey: self.certificatePath ? : @"(null)", CRHTTPServerCertificateKeyPathKey: self.certificateKeyPath ? : @"(null)"}];
        SecKeychainDelete(keychain);
        return nil;
    }

    // Check if the first item in the import is a privatekey
    SecKeyRef key = (SecKeyRef) CFArrayGetValueAtIndex(keyImportItems, 0);
    if ( CFGetTypeID(key) != SecKeyGetTypeID() ) {
        *error = [[NSError alloc] initWithDomain:CRHTTPServerErrorDomain code:CRHTTPServerInvalidCertificatePrivateKey userInfo:@{NSLocalizedDescriptionKey:[NSString stringWithFormat: NSLocalizedString(@"Expected a RSA private key, but got %@ instead.",), (__bridge id)key], CRHTTPServerCertificatePathKey: self.certificatePath ? : @"(null)", CRHTTPServerCertificateKeyPathKey: self.certificateKeyPath ? : @"(null)"}];
        SecKeychainDelete(keychain);
        return nil;
    }

    // Create an identity from the keychain and the certificate
    SecIdentityRef identity;
    OSStatus identityCreationStatus = SecIdentityCreateWithCertificate(keychain, certificate, &identity);
    if ( identityCreationStatus != errSecSuccess ) {
        *error = [[NSError alloc] initWithDomain:CRHTTPServerErrorDomain code:CRHTTPServerInvalidCertificatePrivateKey userInfo:@{NSLocalizedDescriptionKey:[NSString stringWithFormat: NSLocalizedString(@"Unable to get a suitable identity. %@",), (__bridge NSString *)SecCopyErrorMessageString(identityCreationStatus, NULL)], CRHTTPServerCertificatePathKey: self.certificatePath ? : @"(null)", CRHTTPServerCertificateKeyPathKey: self.certificateKeyPath ? : @"(null)"}];
        SecKeychainDelete(keychain);
        return nil;
    }

    // Create the outut array
    NSMutableArray *result = [NSMutableArray arrayWithCapacity:CFArrayGetCount(certificateImportItems)];
    [result addObjectsFromArray:(__bridge NSArray * _Nonnull)(certificateImportItems)];
    result[0] = (__bridge id _Nonnull)(identity);

    // Cleanup
    SecKeychainDelete(keychain);
    
    return result;
}
#endif
@end
