"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const PathToRegexpRouteMatcher_1 = require("@chubbyjs/chubbyjs-framework-router-path-to-regexp/dist/PathToRegexpRouteMatcher");
const Application_1 = require("@chubbyjs/chubbyjs-framework/dist/Application");
const ErrorMiddleware_1 = require("@chubbyjs/chubbyjs-framework/dist/Middleware/ErrorMiddleware");
const RouteMatcherMiddleware_1 = require("@chubbyjs/chubbyjs-framework/dist/Middleware/RouteMatcherMiddleware");
const CallbackRequestHandler_1 = require("@chubbyjs/chubbyjs-framework/dist/RequestHandler/CallbackRequestHandler");
const Route_1 = require("@chubbyjs/chubbyjs-framework/dist/Router/Route");
const Routes_1 = require("@chubbyjs/chubbyjs-framework/dist/Router/Routes");
const ResponseFactory_1 = require("@chubbyjs/chubbyjs-http-message/dist/Factory/ResponseFactory");
const ServerRequestFactory_1 = require("@chubbyjs/chubbyjs-http-message/dist/Factory/ServerRequestFactory");
const StreamFactory_1 = require("@chubbyjs/chubbyjs-http-message/dist/Factory/StreamFactory");
const UriFactory_1 = require("@chubbyjs/chubbyjs-http-message/dist/Factory/UriFactory");
const NodeResponseEmitter_1 = require("@chubbyjs/chubbyjs-node-psr-http-message-bridge/dist/NodeResponseEmitter");
const PsrRequestFactory_1 = require("@chubbyjs/chubbyjs-node-psr-http-message-bridge/dist/PsrRequestFactory");
const http_1 = require("http");
const responseFactory = new ResponseFactory_1.default();
const app = new Application_1.default([
    new ErrorMiddleware_1.default(responseFactory, true),
    new RouteMatcherMiddleware_1.default(new PathToRegexpRouteMatcher_1.default(new Routes_1.default([
        Route_1.default.get('/', 'index', new CallbackRequestHandler_1.default(() => {
            const response = responseFactory.createResponse(200);
            response.getBody().end('');
            return response;
        })),
        Route_1.default.get('/user/:id', 'user_view', new CallbackRequestHandler_1.default((request) => {
            const response = responseFactory.createResponse(200);
            response.getBody().end(request.getAttribute('id'));
            return response;
        })),
        Route_1.default.post('/user', 'user_list', new CallbackRequestHandler_1.default(() => {
            const response = responseFactory.createResponse(200);
            response.getBody().end('');
            return response;
        })),
    ])), responseFactory),
]);
const psrRequestFactory = new PsrRequestFactory_1.default(new ServerRequestFactory_1.default(), new UriFactory_1.default(), new StreamFactory_1.default());
const nodeResponseEmitter = new NodeResponseEmitter_1.default();
const server = (0, http_1.createServer)((req, res) => {
    const serverRequest = psrRequestFactory.create(req);
    const response = app.handle(serverRequest);
    nodeResponseEmitter.emit(response, res);
});
server.listen(3000);
