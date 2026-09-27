import { createRequire } from 'node:module';

// Use the CommonJS entry point to bypass the broken ESM wrapper in 20.71.0
// (fixed by https://github.com/uNetworking/uWebSockets.js/pull/1320, not yet in a tagged release).
const require = createRequire(import.meta.url);
const { App, DeclarativeResponse, _cfg } = require('uWebSockets.js');

// Drop the "uWebSockets: 20" response header
_cfg('silent');

// Create a new instance of uWebSockets.js HTTP
const port = 3000;
const app = App();

// Implementation based on https://github.com/uNetworking/uWebSockets.js/blob/master/examples/Benchmarker.mjs

// GET "/" => 200 with empty body
app.get('/', new DeclarativeResponse().end());

// GET "/user/:id" => 200 with "id" as body
app.get('/user/:id', new DeclarativeResponse().writeParameterValue('id').end());

// POST "/user" => 200 with empty body
app.post('/user', new DeclarativeResponse().end());

// Start the server on port 3000
app.listen(port, (token) => {
  if (!token) {
    console.error(`Failed to listen on port ${port}`);
    process.exit(1);
  }
});
