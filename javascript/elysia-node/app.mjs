import { Elysia } from 'elysia';
import { node } from '@elysiajs/node'

new Elysia({ adapter: node() })
  .get('/', '')
  .get('/user/:id', (req) => req.params.id)
  .post('/user', '')
  .listen({ port: 3000, reusePort: true });
