//
//  CRViewController.m
//  Criollo
//
//  Created by Cătălin Stan on 5/17/14.
//  Copyright (c) 2014 Catalin Stan. All rights reserved.
//

#import "CRViewController.h"
#import "CRRouter_Internal.h"
#import "CRView.h"
#import "CRNib.h"
#import "CRRequest.h"
#import "CRResponse.h"
#import "NSString+Criollo.h"

NS_ASSUME_NONNULL_BEGIN

@interface CRViewController ()

@property (nonatomic, strong, nonnull, readonly) CRNib* nib;

- (void)loadView;

@end

NS_ASSUME_NONNULL_END

@implementation CRViewController


- (instancetype)init {
    return [self initWithNibName:nil bundle:nil prefix:CRPathSeparator];
}

- (instancetype)initWithPrefix:(NSString *)prefix {
    return [self initWithNibName:nil bundle:nil prefix:prefix];
}

- (instancetype)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    return [self initWithNibName:nibNameOrNil bundle:nibBundleOrNil prefix:CRPathSeparator];
}

- (instancetype)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil prefix:(NSString *)prefix {
    self = [super initWithPrefix:prefix];
    if ( self != nil ) {
        _nibName = nibNameOrNil ? : NSStringFromClass(self.class);
        _nibBundle = nibBundleOrNil ? : [NSBundle mainBundle];
        _vars = [NSMutableDictionary dictionary];

        CRViewController* __weak controller = self;
        self.routeBlock = ^(CRRequest *request, CRResponse *response, CRRouteCompletionBlock completionHandler) { @autoreleasepool {
            NSString* requestedPath = request.env[@"DOCUMENT_URI"];
            NSString* requestedRelativePath = [requestedPath pathRelativeToPath:controller.prefix];
            NSArray<CRRouteMatchingResult * >* routes = [controller routesForPath:requestedRelativePath method:request.method];
            [controller executeRoutes:routes forRequest:request response:response withCompletion:completionHandler notFoundBlock:^(CRRequest * _Nonnull req, CRResponse * _Nonnull res, CRRouteCompletionBlock  _Nonnull completion) { @autoreleasepool {
                [res setValue:@"text/html; charset=utf-8" forHTTPHeaderField:@"Content-type"];

                NSString* output = [controller presentViewControllerWithRequest:req response:res];
                if ( controller.shouldFinishResponse ) {
                    [res sendString:output];
                } else {
                    [res writeString:output];
                }

                completion();
            }}];
        }};
    }
    return self;
}

- (void)loadView {
    _nib = [[CRNib alloc] initWithNibNamed:self.nibName bundle:self.nibBundle];
    NSString * contents = [[NSString alloc] initWithBytesNoCopy:(void *)self.nib.data.bytes length:self.nib.data.length encoding:NSUTF8StringEncoding freeWhenDone:NO];

    // Determine the view class to use
    Class viewClass = NSClassFromString([NSStringFromClass(self.class) stringByReplacingOccurrencesOfString:@"Controller" withString:@""]);
    if ( viewClass == nil ) {
        viewClass = [CRView class];
    }
    self.view = [[viewClass alloc] initWithContents:contents];

    //
    [self viewDidLoad];
}

- (void)viewDidLoad {}

- (NSString *)presentViewControllerWithRequest:(CRRequest *)request response:(CRResponse *)response {
    if ( self.view == nil ) {
        [self loadView];
    }
    return [self.view render:self.vars];
}

- (BOOL)shouldFinishResponse {
    return YES;
}

@end
