import engine from '@morojs/engine';

// Raw @morojs/engine, no framework on top: the same role javascript/uwebsockets
// plays for uWebSockets.js. The engine parses HTTP natively and hands each
// request to onRequest as (reqId, methodIdx, path); the reply goes back
// through respond().

// Method indices in the engine's table: GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS, OTHER
const GET = 0;
const POST = 1;
const USER_PREFIX = '/user/';

const server = engine.serve(
  {
    // Reached only by what the engine does not answer itself (see below):
    // GET "/user/:id" on an engine without parameter routes, and anything
    // unmatched, which is a 404.
    onRequest(reqId, methodIdx, path) {
      if (methodIdx === GET && path.startsWith(USER_PREFIX)) {
        const id = path.slice(USER_PREFIX.length);
        if (id.length > 0 && !id.includes('/')) {
          engine.respond(reqId, 200, null, id);
          return;
        }
      }
      engine.respond(reqId, 404, null, null);
    },
    onAborted() {},
    onWritable() {},
  },
  // Every cluster worker binds the port itself (SO_REUSEPORT)
  { reusePort: true }
);

// GET "/" and POST "/user" => 200 with empty body. Fixed replies are the
// engine's static routes: answered inside the engine, no JS per request.
engine.setStaticRoute(server, GET, '/', 200, null, '');
engine.setStaticRoute(server, POST, '/user', 200, null, '');

// GET "/user/:id" => 200 with "id" as body. On an engine with parameter
// routes (>= 1.1.9) the segment after "/user/" is echoed inside the engine
// too, so no request of this benchmark enters JS; older engines take the
// onRequest path above.
if (engine.probe().capabilities?.paramRoutes) {
  engine.setParamRoute(server, GET, USER_PREFIX, '', 200, null);
}

// Start the server on port 3000
engine.listen(server, '0.0.0.0', 3000);
