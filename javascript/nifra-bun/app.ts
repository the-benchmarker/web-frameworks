import { server } from '@nifrajs/core/server';

server()
  .get('/', () => new Response(''))
  .get('/user/:id', (c) => new Response(c.params.id))
  .post('/user', () => new Response(''))
  .listen(3000, { reusePort: true });
