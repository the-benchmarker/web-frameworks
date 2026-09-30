import { server } from '@nifrajs/core/server';
import { serve } from '@nifrajs/node';

const app = server()
  .get('/', () => new Response(''))
  .get('/user/:id', (c) => new Response(c.params.id))
  .post('/user', () => new Response(''));

await serve(app, { port: 3000 });
