import { server } from '@nifrajs/core/server';
import { serve } from '@nifrajs/node';

const app = server()
  .get('/', () => '')
  .get('/user/:id', (c) => c.params.id)
  .post('/user', () => '');

await serve(app, { port: 3000 });
