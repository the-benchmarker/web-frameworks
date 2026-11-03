import { server } from '@nifrajs/core/server';

server()
  .get('/', (c) => c.text(''))
  .get('/user/:id', (c) => c.text(c.params.id))
  .post('/user', (c) => c.text(''))
  .listen(3000, { reusePort: true });
