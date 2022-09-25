import { createApplication } from '@chubbyts/chubbyts-framework/dist/application';
import { createErrorMiddleware } from '@chubbyts/chubbyts-framework/dist/middleware/error-middleware';
import { createRouteMatcherMiddleware } from '@chubbyts/chubbyts-framework/dist/middleware/route-matcher-middleware';
import { createGetRoute, createPostRoute } from '@chubbyts/chubbyts-framework/dist/router/route';
import { createRoutesByName } from '@chubbyts/chubbyts-framework/dist/router/routes-by-name';
import {
  createServerRequestFactory,
  createStreamFromResourceFactory,
  createUriFactory,
  createResponseFactory,
} from '@chubbyts/chubbyts-http/dist/message-factory';
import { createPathToRegexpRouteMatcher } from '@chubbyts/chubbyts-framework-router-path-to-regexp/dist/path-to-regexp-router';
import { Response, ServerRequest } from '@chubbyts/chubbyts-http-types/dist/message';
import { createResponseToUwebsocketsEmitter, createUwebsocketsToServerRequestFactory } from '@chubbyts/chubbyts-uwebsockets-http-bridge/dist/uwebsocket-http';
import { App, HttpRequest, HttpResponse } from 'uWebSockets.js';

const responseFactory = createResponseFactory();

const app = createApplication([
  createErrorMiddleware(responseFactory, true),
  createRouteMatcherMiddleware(
    createPathToRegexpRouteMatcher(
      createRoutesByName([
        createGetRoute({
          path: '/',
          name: 'index',
          handler: async (): Promise<Response> => {
            const response = responseFactory(200);
            response.body.end();

            return response;
          },
        }),
        createGetRoute({
          path: '/user/:id',
          name: 'user_view',
          handler: async (request: ServerRequest): Promise<Response> => {
            const response = responseFactory(200);
            response.body.end(request.attributes.id);

            return response;
          },
        }),
        createPostRoute({
          path: '/user',
          name: 'user_create',
          handler: async (): Promise<Response> => {
            const response = responseFactory(200);
            response.body.end();

            return response;
          },
        }),
      ]),
    ),
  ),
]);

const uwebsocketsToServerRequestFactory = createUwebsocketsToServerRequestFactory(
  createUriFactory(),
  createServerRequestFactory(),
  createStreamFromResourceFactory(),
);

const responseToUwebsocketsEmitter = createResponseToUwebsocketsEmitter();

const host = '0.0.0.0';
const port = 3000;

App()
  .any('/*', async (res: HttpResponse, req: HttpRequest) => {
    // function gets excuted on abort
    // empty function means the request/response gets executed to its end
    res.onAborted(() => { });
    responseToUwebsocketsEmitter(await app(uwebsocketsToServerRequestFactory(req, res)), res);
  })
  .listen(host, port, (listenSocket: unknown) => {
    if (listenSocket) {
      console.log(`Listening to ${host}:${port}`);
    }
  });
