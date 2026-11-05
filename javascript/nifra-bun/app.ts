import { server, staticResponse } from '@nifrajs/core/server';

server()
  .get('/', staticResponse())
  .get('/user/:id', (c) => c.text(c.params.id))
  .post('/user', staticResponse())
  .listen(3000, { reusePort: true });
