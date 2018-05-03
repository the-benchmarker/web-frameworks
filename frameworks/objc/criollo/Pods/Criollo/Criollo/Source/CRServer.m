//
//  CRServer.m
//  Criollo
//
//  Created by Catalin Stan on 7/24/15.
//  Copyright (c) 2015 Cătălin Stan. All rights reserved.
//

#import "CRServer.h"
#import "CRServer_Internal.h"
#import "CRRouter_Internal.h"
#import "CRServerConfiguration.h"
#import "GCDAsyncSocket.h"
#import "CRConnection.h"
#import "CRConnection_Internal.h"
#import "CRMessage_Internal.h"
#import "CRRequest.h"
#import "CRResponse.h"
#import "CRRoute.h"
#import "CRViewController.h"

NS_ASSUME_NONNULL_BEGIN

@interface CRServer () <GCDAsyncSocketDelegate, CRConnectionDelegate>

@property (nonatomic, strong) GCDAsyncSocket* socket;
@property (nonatomic, strong) dispatch_queue_t isolationQueue;
@property (nonatomic, strong) dispatch_queue_t socketDelegateQueue;
@property (nonatomic, strong) dispatch_queue_t acceptedSocketDelegateTargetQueue;

@property (nonatomic, strong) NSOperationQueue* workerQueue;

- (CRConnection *)newConnectionWithSocket:(GCDAsyncSocket *)socket;

@end
NS_ASSUME_NONNULL_END

@implementation CRServer

- (instancetype)init {
    return [self initWithDelegate:nil delegateQueue:nil];
}

- (instancetype)initWithDelegate:(id<CRServerDelegate>)delegate {
    return [self initWithDelegate:delegate delegateQueue:nil];
}

- (instancetype)initWithDelegate:(id<CRServerDelegate>)delegate delegateQueue:(dispatch_queue_t)delegateQueue {
    self = [super init];
    if ( self != nil ) {
        _configuration = [[CRServerConfiguration alloc] init];
        _delegate = delegate;
        _delegateQueue = delegateQueue;
        if ( _delegateQueue == nil ) {
            _delegateQueue = dispatch_queue_create([[[NSBundle mainBundle].bundleIdentifier stringByAppendingPathExtension:@"ServerDelegateQueue"] cStringUsingEncoding:NSASCIIStringEncoding], DISPATCH_QUEUE_SERIAL);
            dispatch_set_target_queue(_delegateQueue, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0));
        }
    }
    return self;
}

#pragma mark - Listening

- (BOOL)startListening {
    return [self startListening:nil portNumber:0 interface:nil];
}

- (BOOL)startListening:(NSError *__autoreleasing *)error {
    return [self startListening:error portNumber:0 interface:nil];
}

- (BOOL)startListening:(NSError *__autoreleasing *)error portNumber:(NSUInteger)portNumber {
    return [self startListening:error portNumber:portNumber interface:nil];
}

- (BOOL)startListening:(NSError *__autoreleasing *)error portNumber:(NSUInteger)portNumber interface:(NSString *)interface {

    if ( portNumber != 0 ) {
        self.configuration.CRServerPort = portNumber;
    }

    if ( interface.length != 0 ) {
        self.configuration.CRServerInterface = interface;
    }

    self.isolationQueue = dispatch_queue_create([[[NSBundle mainBundle].bundleIdentifier stringByAppendingPathExtension:@"ServerIsolationQueue"] cStringUsingEncoding:NSASCIIStringEncoding], DISPATCH_QUEUE_SERIAL);
    dispatch_set_target_queue(self.isolationQueue, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0));

    self.socketDelegateQueue = dispatch_queue_create([[[NSBundle mainBundle].bundleIdentifier stringByAppendingPathExtension:@"SocketDelegateQueue"] cStringUsingEncoding:NSASCIIStringEncoding], DISPATCH_QUEUE_CONCURRENT);
    dispatch_set_target_queue(self.socketDelegateQueue, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0));

    self.acceptedSocketDelegateTargetQueue = dispatch_queue_create([[[NSBundle mainBundle].bundleIdentifier stringByAppendingPathExtension:@"AcceptedSocketDelegateTargetQueue"] cStringUsingEncoding:NSASCIIStringEncoding], DISPATCH_QUEUE_SERIAL);
    dispatch_set_target_queue(self.acceptedSocketDelegateTargetQueue, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0));

    self.workerQueue = [[NSOperationQueue alloc] init];
    if ( [self.workerQueue respondsToSelector:@selector(qualityOfService)] ) {
        self.workerQueue.qualityOfService = NSQualityOfServiceUserInitiated;
    }
    self.workerQueue.maxConcurrentOperationCount = NSOperationQueueDefaultMaxConcurrentOperationCount;

    self.connections = [NSMutableArray array];
    self.socket = [[GCDAsyncSocket alloc] initWithDelegate:self delegateQueue:self.socketDelegateQueue];

    if ( [self.delegate respondsToSelector:@selector(serverWillStartListening:)] ) {
        dispatch_async(self.delegateQueue, ^{
            [self.delegate serverWillStartListening:self];
        });
    }

    BOOL listening = [self.socket acceptOnInterface:self.configuration.CRServerInterface port:self.configuration.CRServerPort error:error];
    if ( listening && [self.delegate respondsToSelector:@selector(serverDidStartListening:)] ) {
        dispatch_async(self.delegateQueue, ^{
            [self.delegate serverDidStartListening:self];
        });
    }

    return listening;
}

- (void)stopListening {

    if ( [self.delegate respondsToSelector:@selector(serverWillStopListening:)] ) {
        dispatch_async(self.delegateQueue, ^{
            [self.delegate serverWillStopListening:self];
        });
    }

    [self.workerQueue cancelAllOperations];
    [self.socket disconnect];

    if ( [self.delegate respondsToSelector:@selector(serverDidStopListening:)] ) {
        dispatch_async(self.delegateQueue, ^{
            [self.delegate serverDidStopListening:self];
        });
    }
    
}

#pragma mark - Connections

- (void)closeAllConnections:(dispatch_block_t)completion {
    CRServer * __weak server = self;
    dispatch_async(self.isolationQueue ? : dispatch_get_main_queue(), ^{ @autoreleasepool {
        [server.connections enumerateObjectsUsingBlock:^(CRConnection * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) { @autoreleasepool {
            [obj.socket disconnectAfterReadingAndWriting];
        }}];
        [server.connections removeAllObjects];
        if ( completion ) {
            dispatch_async(server.delegateQueue, completion);
        }
    }});
}

- (CRConnection*)newConnectionWithSocket:(GCDAsyncSocket*)socket {
    return [[CRConnection alloc] initWithSocket:socket server:self];
}

#pragma mark - GCDAsyncSocketDelegate

- (void)socket:(GCDAsyncSocket *)sock didAcceptNewSocket:(GCDAsyncSocket *)newSocket {
    dispatch_queue_t acceptedSocketDelegateQueue = dispatch_queue_create([[[NSBundle mainBundle].bundleIdentifier stringByAppendingPathExtension:[NSString stringWithFormat:@"SocketDelegateQueue-%hu", newSocket.connectedPort]] cStringUsingEncoding:NSASCIIStringEncoding], DISPATCH_QUEUE_CONCURRENT);
    dispatch_set_target_queue(acceptedSocketDelegateQueue, self.acceptedSocketDelegateTargetQueue);
    newSocket.delegateQueue = acceptedSocketDelegateQueue;

    CRConnection* connection = [self newConnectionWithSocket:newSocket];
    connection.delegate = self;

    CRServer * __weak server = self;
    dispatch_async(self.isolationQueue, ^(){
        [server.connections addObject:connection];
    });
    if ( [self.delegate respondsToSelector:@selector(server:didAcceptConnection:)]) {
        dispatch_async(self.delegateQueue, ^{
            [server.delegate server:server didAcceptConnection:connection];
        });
    }
    [connection startReading];
}

#pragma mark - CRConnectionDelegate

- (void)connection:(CRConnection *)connection didReceiveRequest:(CRRequest *)request response:(CRResponse *)response {
    CRServer * __weak server = self;
    [self.workerQueue addOperationWithBlock:^{ @autoreleasepool {
        NSArray<CRRouteMatchingResult *> * routes = [server routesForPath:request.URL.path method:request.method];
        [server executeRoutes:routes forRequest:request response:response withCompletion:^{} notFoundBlock:server.notFoundBlock];
    }}];
    if ( [self.delegate respondsToSelector:@selector(server:didReceiveRequest:)] ) {
        dispatch_async(self.delegateQueue, ^{ @autoreleasepool {
            [self.delegate server:server didReceiveRequest:request];
        }});
    }
}

- (void)connection:(CRConnection *)connection didFinishRequest:(CRRequest *)request response:(CRResponse *)response {
    if ( [self.delegate respondsToSelector:@selector(server:didFinishRequest:)]  ) {
        CRServer * __weak server = self;
        dispatch_async(self.delegateQueue, ^{
            [server.delegate server:server didFinishRequest:request];
        });
    }
}

- (void)didCloseConnection:(CRConnection*)connection {
    CRServer * __weak server = self;
    if ( [self.delegate respondsToSelector:@selector(server:didCloseConnection:)]) {
        dispatch_async(self.delegateQueue, ^{
            [server.delegate server:server didCloseConnection:connection];
        });
    }
    dispatch_async(self.isolationQueue, ^(){
        [server.connections removeObject:connection];
    });
}

@end
