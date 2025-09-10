"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const chubbyts_http_node_bridge_1 = require("@chubbyts/chubbyts-http-node-bridge");
const application_1 = require("@chubbyts/chubbyts-framework/dist/application");
const error_middleware_1 = require("@chubbyts/chubbyts-framework/dist/middleware/error-middleware");
const route_matcher_middleware_1 = require("@chubbyts/chubbyts-framework/dist/middleware/route-matcher-middleware");
const route_1 = require("@chubbyts/chubbyts-framework/dist/router/route");
const routes_by_name_1 = require("@chubbyts/chubbyts-framework/dist/router/routes-by-name");
const message_factory_1 = require("@chubbyts/chubbyts-http/dist/message-factory");
const path_to_regexp_router_1 = require("@chubbyts/chubbyts-framework-router-path-to-regexp/dist/path-to-regexp-router");
const node_http_1 = require("@chubbyts/chubbyts-framework/dist/server/node-http");
const responseFactory = (0, message_factory_1.createResponseFactory)();
const app = (0, application_1.createApplication)([
    (0, error_middleware_1.createErrorMiddleware)(responseFactory, true),
    (0, route_matcher_middleware_1.createRouteMatcherMiddleware)((0, path_to_regexp_router_1.createPathToRegexpRouteMatcher)((0, routes_by_name_1.createRoutesByName)([
        (0, route_1.createGetRoute)({
            path: "/",
            name: "index",
            handler: async () => {
                const response = responseFactory(200);
                response.body.end();
                return response;
            },
        }),
        (0, route_1.createGetRoute)({
            path: "/user/:id",
            name: "user_view",
            handler: async (request) => {
                const response = responseFactory(200);
                response.body.end(request.attributes.id);
                return response;
            },
        }),
        (0, route_1.createPostRoute)({
            path: "/user",
            name: "user_create",
            handler: async () => {
                const response = responseFactory(200);
                response.body.end();
                return response;
            },
        }),
    ]))),
]);
const nodeToServerRequestFactory = (0, node_http_1.createNodeToServerRequestFactory)((0, message_factory_1.createUriFactory)(), (0, message_factory_1.createServerRequestFactory)(), (0, message_factory_1.createStreamFromResourceFactory)());
const responseToNodeEmitter = (0, node_http_1.createResponseToNodeEmitter)();
const server = (0, chubbyts_http_node_bridge_1.createServer)(async (req, res) => {
    responseToNodeEmitter(await app(nodeToServerRequestFactory(req)), res);
});
const host = "0.0.0.0";
const port = 3000;
server.listen(port, host);
