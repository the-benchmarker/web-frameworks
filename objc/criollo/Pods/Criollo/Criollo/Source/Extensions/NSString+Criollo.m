//
//  NSString+Criollo.m
//  Criollo
//
//  Created by Cătălin Stan on 4/12/14.
//  Copyright (c) 2014 Catalin Stan. All rights reserved.
//

#import "NSString+Criollo.h"
#import "CRTypes.h"

@implementation NSString (Criollo)

- (NSString *)stringByDecodingURLEncodedString {
	NSString* returnString;
	if ( [self respondsToSelector:@selector(stringByRemovingPercentEncoding)] ) {
		returnString = self.stringByRemovingPercentEncoding;
	} else {
		CFStringRef encodedCFString = (__bridge CFStringRef)self;
		CFStringRef returnCFString = CFURLCreateStringByReplacingPercentEscapesUsingEncoding(NULL, encodedCFString, (CFStringRef)@"!*'();:&=$,/?%#[]", kCFStringEncodingUTF8);
		returnString = (__bridge_transfer NSString*)returnCFString;
		
	}
	if ( returnString == nil ) {
		returnString = [self stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
	}
	if ( returnString == nil ) {
		returnString = self;
	}
    returnString = [returnString stringByReplacingOccurrencesOfString:@"+" withString:@" "];
    return returnString;
}

- (NSString *)URLEncodedString {
	NSString* returnString;
	NSString* allowedCharacters = @"!*'();:&=$,/?%#[]";
	
	if ( [self respondsToSelector:@selector(stringByAddingPercentEncodingWithAllowedCharacters:)] ) {
		returnString = [self stringByAddingPercentEncodingWithAllowedCharacters:[NSCharacterSet characterSetWithCharactersInString:allowedCharacters]];
	} else {
		CFStringRef encodedCFString = (__bridge CFStringRef)self;
		CFStringRef returnCFString = CFURLCreateStringByAddingPercentEscapes(NULL, encodedCFString, NULL, (CFStringRef)allowedCharacters, kCFStringEncodingUTF8 );
		returnString = (__bridge_transfer NSString *)returnCFString;
	}
	
	if ( returnString == nil ) {
		returnString = [self stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
	}
	if ( returnString == nil ) {
		returnString = self;
	}
	
    returnString = [returnString stringByReplacingOccurrencesOfString:@" " withString:@"%20"];
    return returnString;
}

- (NSString *)uppercaseFirstLetterString {
    return [[self substringToIndex:1].uppercaseString stringByAppendingString:[self substringFromIndex:1].lowercaseString];
}

- (NSString *)stringbyFormattingHTTPHeader {
    NSMutableArray* words = [[self componentsSeparatedByString:@"-"] mutableCopy];
    [words enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
        [words setObject:[obj uppercaseFirstLetterString] atIndexedSubscript:idx];
    }];
    return [words componentsJoinedByString:@"-"];
}

- (NSString *)pathRelativeToPath:(NSString *)path {
    
    if ( [path isEqualToString:CRPathSeparator] ) {
        return self;
    }

    NSUInteger relativePathStart = [self rangeOfString:path options:NSBackwardsSearch].location;
    if ( relativePathStart == NSNotFound ) {
        relativePathStart = 0;
    }

    NSString * relativePath;
    @try {
        relativePath = [[self substringFromIndex:relativePathStart + path.length] stringByStandardizingPath];
    } @catch (NSException *exception) {
        relativePath = @"";
    }

    if ( ![relativePath hasPrefix:CRPathSeparator] ) {
        relativePath = [CRPathSeparator stringByAppendingString:relativePath ? : @""];
    }

    return relativePath;
}

@end
