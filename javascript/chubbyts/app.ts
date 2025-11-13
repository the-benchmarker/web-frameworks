import { createApplication } from '@chubbyts/chubbyts-framework/dist/application';
import { createErrorMiddleware } from '@chubbyts/chubbyts-framework/dist/middleware/error-middleware';
import { createRouteMatcherMiddleware } from '@chubbyts/chubbyts-framework/dist/middleware/route-matcher-middleware';
import { createGetRoute, createPostRoute } from '@chubbyts/chubbyts-framework/dist/router/route';
import { createRoutesByName } from '@chubbyts/chubbyts-framework/dist/router/routes-by-name';
import { createPathToRegexpRouteMatcher } from '@chubbyts/chubbyts-framework-router-path-to-regexp/dist/path-to-regexp-router';
import { createServer, STATUS_CODES } from 'http';
import { Response, ServerRequest } from '@chubbyts/chubbyts-undici-server/dist/server';
import { createNodeRequestToUndiciRequestFactory, createUndiciResponseToNodeResponseEmitter } from '@chubbyts/chubbyts-undici-server-node/dist/node';

const app = createApplication([
  createErrorMiddleware(true),
  createRouteMatcherMiddleware(
    createPathToRegexpRouteMatcher(
      createRoutesByName([
        createGetRoute({
          path: '/',
          name: 'index',
          handler: async (): Promise<Response> => {
            return new Response(null, {
              status: 200,
              statusText: STATUS_CODES[200],
            });
          },
        }),
        createGetRoute({
          path: '/user/:id',
          name: 'user_view',
          handler: async (request: ServerRequest<{id: string}>): Promise<Response> => {
            return new Response(request.attributes.id, {
              status: 200,
              statusText: STATUS_CODES[200],
            });
          },
        }),
        createPostRoute({
          path: '/user',
          name: 'user_create',
          handler: async (): Promise<Response> => {
            return new Response(null, {
              status: 200,
              statusText: STATUS_CODES[200],
            });
          },
        }),
      ]),
    ),
  ),
]);

const nodeRequestToUndiciRequestFactory = createNodeRequestToUndiciRequestFactory();
const undiciResponseToNodeResponseEmitter = createUndiciResponseToNodeResponseEmitter();

const server = createServer(async (req, res) => {
  undiciResponseToNodeResponseEmitter(await app(nodeRequestToUndiciRequestFactory(req)), res);
});

const host = '0.0.0.0';
const port = 3000;

server.listen(port, host);
