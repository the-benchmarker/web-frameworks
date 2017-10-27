//
//  NSDate+RFC1123.h
//  MKNetworkKit
//
//  Created by Marcus Rohrmoser
//  http://blog.mro.name/2009/08/nsdateformatter-http-header/
//  http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.3.1

//  No obvious license attached

/**
 Convert RFC1123 format dates
 
 */

@interface NSDate (RFC1123)

/**
 * @name Convert a string to NSDate
 */

/**
 * Convert a RFC1123 'Full-Date' string
 * (http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.3.1)
 * into NSDate.
 *
 * @param value_ NSString* the string to convert
 * @return NSDate*
 */
+(NSDate*)dateFromRFC1123:(NSString*)value_;

/**
 * @name Retrieve NSString value of the current date
 */

/**
 * Convert NSDate into a RFC1123 'Full-Date' string
 * (http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.3.1).
 *
 * @return NSString*
 */
@property (nonatomic, readonly, copy) NSString *rfc1123String;

@end
