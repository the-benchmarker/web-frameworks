//
//  CRMessage.h
//  Criollo
//
//  Created by Cătălin Stan on 29/04/15.
//
//

#import "CRTypes.h"

NS_ASSUME_NONNULL_BEGIN

FOUNDATION_EXTERN NSString * NSStringFromCRHTTPVersion(CRHTTPVersion version);
FOUNDATION_EXTERN CRHTTPVersion CRHTTPVersionMake(NSString * versionSpec);

FOUNDATION_EXTERN NSString * NSStringFromCRHTTPMethod(CRHTTPMethod method);
FOUNDATION_EXTERN CRHTTPMethod CRHTTPMethodMake(NSString * methodSpec);

@interface CRMessage : NSObject

@property (nonatomic, readonly) CRHTTPVersion version;
@property (nonatomic, readonly) NSDictionary<NSString*, NSString*>* allHTTPHeaderFields;

@property (nonatomic, copy, nullable) NSData* bodyData;

- (nullable NSString *)valueForHTTPHeaderField:(NSString *)HTTPHeaderField;

@end

NS_ASSUME_NONNULL_END
