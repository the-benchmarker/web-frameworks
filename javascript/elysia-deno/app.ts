import { Elysia } from 'elysia';

const app = new Elysia()
  .get('/', '')
  .get('/user/:id', (req) => req.params.id)
  .post('/user', '');

export default { fetch: app.fetch };
