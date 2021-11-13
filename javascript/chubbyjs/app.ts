import PathToRegexpRouteMatcher from "@chubbyjs/chubbyjs-framework-router-path-to-regexp/dist/PathToRegexpRouteMatcher";
import Application from "@chubbyjs/chubbyjs-framework/dist/Application";
import ErrorMiddleware from "@chubbyjs/chubbyjs-framework/dist/Middleware/ErrorMiddleware";
import RouteMatcherMiddleware from "@chubbyjs/chubbyjs-framework/dist/Middleware/RouteMatcherMiddleware";
import CallbackRequestHandler from "@chubbyjs/chubbyjs-framework/dist/RequestHandler/CallbackRequestHandler";
import Route from "@chubbyjs/chubbyjs-framework/dist/Router/Route";
import Routes from "@chubbyjs/chubbyjs-framework/dist/Router/Routes";
import ResponseFactory from "@chubbyjs/chubbyjs-http-message/dist/Factory/ResponseFactory";
import ServerRequestFactory from "@chubbyjs/chubbyjs-http-message/dist/Factory/ServerRequestFactory";
import StreamFactory from "@chubbyjs/chubbyjs-http-message/dist/Factory/StreamFactory";
import UriFactory from "@chubbyjs/chubbyjs-http-message/dist/Factory/UriFactory";
import NodeResponseEmitter from "@chubbyjs/chubbyjs-node-psr-http-message-bridge/dist/NodeResponseEmitter";
import PsrRequestFactory from "@chubbyjs/chubbyjs-node-psr-http-message-bridge/dist/PsrRequestFactory";
import ResponseInterface from "@chubbyjs/psr-http-message/dist/ResponseInterface";
import ServerRequestInterface from "@chubbyjs/psr-http-message/dist/ServerRequestInterface";
import { createServer, IncomingMessage, ServerResponse } from "http";

const responseFactory = new ResponseFactory();

const app = new Application([
  new ErrorMiddleware(responseFactory, true),
  new RouteMatcherMiddleware(
    new PathToRegexpRouteMatcher(
      new Routes([
        Route.get(
          "/",
          "index",
          new CallbackRequestHandler(
            async (): Promise<ResponseInterface> => {
              const response = responseFactory.createResponse(200);
              response.getBody().end("");

              return response;
            }
          )
        ),
        Route.get(
          "/user/:id",
          "user_view",
          new CallbackRequestHandler(
            async (
              request: ServerRequestInterface
            ): Promise<ResponseInterface> => {
              const response = responseFactory.createResponse(200);
              response.getBody().end(request.getAttribute("id"));

              return response;
            }
          )
        ),
        Route.post(
          "/user",
          "user_create",
          new CallbackRequestHandler(
            async (): Promise<ResponseInterface> => {
              const response = responseFactory.createResponse(200);
              response.getBody().end("");

              return response;
            }
          )
        ),
      ])
    ),
    responseFactory
  ),
]);

const psrRequestFactory = new PsrRequestFactory(
  new ServerRequestFactory(),
  new UriFactory(),
  new StreamFactory()
);

const nodeResponseEmitter = new NodeResponseEmitter();

const server = createServer(
  async (req: IncomingMessage, res: ServerResponse) => {
    const serverRequest = psrRequestFactory.create(req);
    const response = await app.handle(serverRequest);

    nodeResponseEmitter.emit(response, res);
  }
);

server.listen(3000, "0.0.0.0");
