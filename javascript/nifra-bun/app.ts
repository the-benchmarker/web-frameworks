import { server, staticResponse } from '@nifrajs/core/server';

server()
  .get('/', staticResponse())
  .get('/user/:id', (c) => new Response(c.params.id))
  .post('/user', staticResponse())
  .listen(3000, { reusePort: true });
