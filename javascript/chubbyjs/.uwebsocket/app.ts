import Application from "@chubbyjs/chubbyjs-framework/dist/Application";
import CallbackRequestHandler from "@chubbyjs/chubbyjs-framework/dist/RequestHandler/CallbackRequestHandler";
import ErrorMiddleware from "@chubbyjs/chubbyjs-framework/dist/Middleware/ErrorMiddleware";
import PathToRegexpRouteMatcher from "@chubbyjs/chubbyjs-framework-router-path-to-regexp/dist/PathToRegexpRouteMatcher";
import PsrRequestFactory from "@chubbyjs/chubbyjs-uwebsockets-psr-http-message-bridge/dist/PsrRequestFactory";
import ResponseFactory from "@chubbyjs/chubbyjs-http-message/dist/Factory/ResponseFactory";
import ResponseInterface from "@chubbyjs/psr-http-message/dist/ResponseInterface";
import Route from "@chubbyjs/chubbyjs-framework/dist/Router/Route";
import RouteMatcherMiddleware from "@chubbyjs/chubbyjs-framework/dist/Middleware/RouteMatcherMiddleware";
import Routes from "@chubbyjs/chubbyjs-framework/dist/Router/Routes";
import ServerRequestFactory from "@chubbyjs/chubbyjs-http-message/dist/Factory/ServerRequestFactory";
import ServerRequestInterface from "@chubbyjs/psr-http-message/dist/ServerRequestInterface";
import StreamFactory from "@chubbyjs/chubbyjs-http-message/dist/Factory/StreamFactory";
import UriFactory from "@chubbyjs/chubbyjs-http-message/dist/Factory/UriFactory";
import UwebsocketResponseEmitter from "@chubbyjs/chubbyjs-uwebsockets-psr-http-message-bridge/dist/UwebsocketResponseEmitter";
import { HttpRequest, HttpResponse } from "uWebSockets.js";

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

const uwebsocketResponseEmitter = new UwebsocketResponseEmitter();

require("uWebSockets.js")
  .App()
  .any("/*", async (res: HttpResponse, req: HttpRequest) => {
    const serverRequest = psrRequestFactory.create(req, res);
    const response = await app.handle(serverRequest);

    uwebsocketResponseEmitter.emit(response, res);
  })
  .listen("0.0.0.0", 3000, (listenSocket: unknown) => {
    if (listenSocket) {
      console.log("Listening to port 0.0.0.0:3000");
    }
  });
