//
//  NSDate+RFC1123.m
//  MKNetworkKit
//
//  Originally created by Marcus Rohrmoser
//  http://blog.mro.name/2009/08/nsdateformatter-http-header/
//  Updated with strptime methods by Bo98
//
//  No obvious license attached

#import "NSDate+RFC1123.h"
#import <time.h>
#import <xlocale.h>

@implementation NSDate (RFC1123)

+(NSDate*)dateFromRFC1123:(NSString*)value_
{
    if(value_ == nil)
        return nil;    
    
    const char *str = [value_ UTF8String];
    const char *fmt;
    NSDate *retDate;
    char *ret;
    
    fmt = "%a, %d %b %Y %H:%M:%S %Z";
    struct tm rfc1123timeinfo;
    memset(&rfc1123timeinfo, 0, sizeof(rfc1123timeinfo));
    ret = strptime_l(str, fmt, &rfc1123timeinfo, NULL);
    if (ret) {
        time_t rfc1123time = mktime(&rfc1123timeinfo);
        retDate = [NSDate dateWithTimeIntervalSince1970:rfc1123time];
        if (retDate != nil)
            return retDate;
    }
    
    
    fmt = "%A, %d-%b-%y %H:%M:%S %Z";
    struct tm rfc850timeinfo;
    memset(&rfc850timeinfo, 0, sizeof(rfc850timeinfo));
    ret = strptime_l(str, fmt, &rfc850timeinfo, NULL);
    if (ret) {
        time_t rfc850time = mktime(&rfc850timeinfo);
        retDate = [NSDate dateWithTimeIntervalSince1970:rfc850time];
        if (retDate != nil)
            return retDate;
    }
    
    fmt = "%a %b %e %H:%M:%S %Y";
    struct tm asctimeinfo;
    memset(&asctimeinfo, 0, sizeof(asctimeinfo));
    ret = strptime_l(str, fmt, &asctimeinfo, NULL);
    if (ret) {
        time_t asctime = mktime(&asctimeinfo);
        return [NSDate dateWithTimeIntervalSince1970:asctime];
    }
    
    return nil;
}

-(NSString*)rfc1123String
{
    time_t date = (time_t)[self timeIntervalSince1970];
    struct tm timeinfo;
    gmtime_r(&date, &timeinfo);
    char buffer[32];
    size_t ret = strftime_l(buffer, sizeof(buffer), "%a, %d %b %Y %H:%M:%S GMT", &timeinfo, NULL);
    if (ret) {
        return @(buffer);
    } else {
        return nil;
    }
}

@end
