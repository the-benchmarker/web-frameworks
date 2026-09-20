import { createRequire } from 'node:module';

// Use the CommonJS entry point to bypass the broken ESM wrapper in 20.71.0.
const require = createRequire(import.meta.url);
const { App } = require('uWebSockets.js');

// Create a new instance of uWebSockets.js HTTP
const port = 3000;
const app = App();

// GET "/" => 200 with empty body
app.get('/', (res, req) => {
  res.end('');
});

// GET "/user/:id" => 200 with "id" as body
app.get('/user/:id', (res, req) => {
  res.end(req.getParameter(0));
});

// POST "/user" => 200 with empty body
app.post('/user', (res, req) => {
  res.end('');
});

// Start the server on port 3000
app.listen(port, () => {});
