import { createRequire } from 'node:module';
import { echoParam, server, staticResponse } from '@nifrajs/core/server';
import { serve } from '@nifrajs/node-uws';

// Drop the "uWebSockets: 20" response header (same switch the uwebsockets entry uses)
createRequire(import.meta.url)('uWebSockets.js')._cfg('silent');

const app = server()
  .get('/', staticResponse())
  .get('/user/:id', echoParam('/user/:id'))
  .post('/user', staticResponse());

await serve(app, { port: 3000 });
