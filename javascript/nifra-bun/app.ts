import { server } from '@nifrajs/core/server';

server()
  .get('/', () => '')
  .get('/user/:id', (c) => c.params.id)
  .post('/user', () => '')
  .listen(3000, { reusePort: true });
