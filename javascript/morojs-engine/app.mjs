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
    // GET "/user/:id" => 200 with "id" as body; anything else unmatched => 404
    onRequest(reqId, methodIdx, path) {
      if (methodIdx === GET && path.startsWith(USER_PREFIX)) {
        engine.respond(reqId, 200, null, path.slice(USER_PREFIX.length));
        return;
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

// Start the server on port 3000
engine.listen(server, '0.0.0.0', 3000);
