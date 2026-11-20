import { echoParam, server, staticResponse } from '@nifrajs/core/server';
import { serve } from '@nifrajs/node-uws';

const app = server()
  .get('/', staticResponse())
  .get('/user/:id', echoParam('/user/:id'))
  .post('/user', staticResponse());

await serve(app, { port: 3000 });
