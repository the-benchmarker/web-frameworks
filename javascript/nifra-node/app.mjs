import { server } from '@nifrajs/core/server';
import { serve } from '@nifrajs/node';

const app = server()
  .get('/', (c) => c.text(''))
  .get('/user/:id', (c) => c.text(c.params.id))
  .post('/user', (c) => c.text(''));

await serve(app, { port: 3000 });
