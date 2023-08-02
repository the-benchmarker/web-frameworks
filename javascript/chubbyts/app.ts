import { createApplication } from "@chubbyts/chubbyts-framework/dist/application";
import { createErrorMiddleware } from "@chubbyts/chubbyts-framework/dist/middleware/error-middleware";
import { createRouteMatcherMiddleware } from "@chubbyts/chubbyts-framework/dist/middleware/route-matcher-middleware";
import {
  createGetRoute,
  createPostRoute,
} from "@chubbyts/chubbyts-framework/dist/router/route";
import { createRoutesByName } from "@chubbyts/chubbyts-framework/dist/router/routes-by-name";
import {
  createServerRequestFactory,
  createStreamFromResourceFactory,
  createUriFactory,
  createResponseFactory,
} from "@chubbyts/chubbyts-http/dist/message-factory";
import { createPathToRegexpRouteMatcher } from "@chubbyts/chubbyts-framework-router-path-to-regexp/dist/path-to-regexp-router";
import {
  Response,
  ServerRequest,
} from "@chubbyts/chubbyts-http-types/dist/message";
import { createServer, IncomingMessage, ServerResponse } from "http";
import {
  createNodeToServerRequestFactory,
  createResponseToNodeEmitter,
} from "@chubbyts/chubbyts-framework/dist/server/node-http";

const responseFactory = createResponseFactory();

const app = createApplication([
  createErrorMiddleware(responseFactory, true),
  createRouteMatcherMiddleware(
    createPathToRegexpRouteMatcher(
      createRoutesByName([
        createGetRoute({
          path: "/",
          name: "index",
          handler: async (): Promise<Response> => {
            const response = responseFactory(200);
            response.body.end();

            return response;
          },
        }),
        createGetRoute({
          path: "/user/:id",
          name: "user_view",
          handler: async (request: ServerRequest): Promise<Response> => {
            const response = responseFactory(200);
            response.body.end(request.attributes.id);

            return response;
          },
        }),
        createPostRoute({
          path: "/user",
          name: "user_create",
          handler: async (): Promise<Response> => {
            const response = responseFactory(200);
            response.body.end();

            return response;
          },
        }),
      ])
    )
  ),
]);

const nodeToServerRequestFactory = createNodeToServerRequestFactory(
  createUriFactory(),
  createServerRequestFactory(),
  createStreamFromResourceFactory()
);

const responseToNodeEmitter = createResponseToNodeEmitter();

const server = createServer(
  async (req: IncomingMessage, res: ServerResponse) => {
    responseToNodeEmitter(await app(nodeToServerRequestFactory(req)), res);
  }
);

const host = "0.0.0.0";
const port = 3000;

server.listen(port, host, () => {
  console.log(`Listening to ${host}:${port}`);
});
