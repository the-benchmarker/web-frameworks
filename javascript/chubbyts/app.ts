import { createApplication } from '@chubbyts/chubbyts-framework/application';
import { createErrorMiddleware } from '@chubbyts/chubbyts-framework/middleware/error-middleware';
import { createRouteMatcherMiddleware } from '@chubbyts/chubbyts-framework/middleware/route-matcher-middleware';
import { createGetRoute, createPostRoute } from '@chubbyts/chubbyts-framework/router/route';
import { createRoutesByName } from '@chubbyts/chubbyts-framework/router/routes-by-name';
import { createPathToRegexpMatch } from '@chubbyts/chubbyts-framework-router-path-to-regexp/path-to-regexp-router';
import {
  createResponseFactory,
  createServerRequestFactory,
  createStreamFactory,
  createUriFactory,
} from '@chubbyts/chubbyts-http/message-factory';
import { createServerRequestHelpers } from '@chubbyts/chubbyts-http/message-helper';
import type { Response, ServerRequest } from '@chubbyts/chubbyts-http-types/message';
import {
  createNodeRequestToServerRequestFactory,
  createResponseToNodeResponseEmitter,
} from '@chubbyts/chubbyts-http-node/node';
import { createServer } from 'http';

const bodyFactory = createStreamFactory();
const responseFactory = createResponseFactory(bodyFactory);

const app = createApplication([
  createErrorMiddleware(responseFactory, bodyFactory, true),
  createRouteMatcherMiddleware(
    createPathToRegexpMatch(
      createRoutesByName([
        createGetRoute({
          path: '/',
          name: 'index',
          handler: async (): Promise<Response> => {
            return responseFactory(200);
          },
        }),
        createGetRoute({
          path: '/user/:id',
          name: 'user_view',
          handler: async (request: ServerRequest): Promise<Response> => {
            return { ...responseFactory(200), body: bodyFactory(request.attributes.id as string) };
          },
        }),
        createPostRoute({
          path: '/user',
          name: 'user_create',
          handler: async (): Promise<Response> => {
            return responseFactory(200);
          },
        }),
      ]),
    ),
  ),
]);

const nodeRequestToServerRequestFactory = createNodeRequestToServerRequestFactory(
  createUriFactory(),
  createServerRequestFactory(),
  createServerRequestHelpers(),
);
const responseToNodeResponseEmitter = createResponseToNodeResponseEmitter();

const server = createServer(async (req, res) => {
  responseToNodeResponseEmitter(await app(nodeRequestToServerRequestFactory(req, res)), res);
});

const host = '0.0.0.0';
const port = 3000;

server.listen(port, host);
