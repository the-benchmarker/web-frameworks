//
//  CRNib.h
//  Criollo
//
//  Created by Cătălin Stan on 5/18/14.
//  Copyright (c) 2014 Catalin Stan. All rights reserved.
//

NS_ASSUME_NONNULL_BEGIN

/**
 Instances of the `NSNib` class serve as object wrappers, or containers, for the
 HTML content serverd by instances of `CRViewController`. An `CRNib` object
 keeps the contents of a nib file resident in memory, ready for instantiation.

 Criollo follows the same paradign as Cocoa and UIKit with regards to
 view controllers and nibs. The main difference is that Criollo expects
 an HTML document as the nib's contents.
 
 ## A note on Templating
 
 If you prefer to use a templating language to write html, the way to do
 it to compile the file at build time and copy it to the bundle's resource
 directory. [Criollo Web](https://github.com/thecatalinstan/Criollo-Web)
 uses `jade` as its template language for example.

 */
@interface CRNib : NSObject

/**
 *  The contents of the nib.
 */
@property (nonatomic, strong, readonly) NSData *data;

/**
 *  The nib name
 */
@property (nonatomic, strong, readonly) NSString *name;

/**
 @name Initializing a CRNib
 */

/**
 Creates a new `CRNib` object from a file qith the given `nibName`,
 in the given bundle.
 
 The method will look for a file named `nibName.html` inside the
 `Resources` folder of the specified bundle.

 @param nibName The base name of the nib file
 @param bundle  The bundle in which to locate the file

 @return A `CRNib` object or `nil` if the file was not found
 */
- (instancetype)initWithNibNamed:(NSString *)nibName bundle:(NSBundle * _Nullable)bundle NS_DESIGNATED_INITIALIZER;

@end

NS_ASSUME_NONNULL_END
