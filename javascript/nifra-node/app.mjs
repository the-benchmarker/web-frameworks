import { server, staticResponse } from '@nifrajs/core/server';
import { serve } from '@nifrajs/node';

const app = server()
  .get('/', staticResponse())
  .get('/user/:id', (c) => new Response(c.params.id))
  .post('/user', staticResponse());

await serve(app, { port: 3000 });
