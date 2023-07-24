import { Router } from '@stricjs/router';  

export default new Router()
  .get('/', () => new Response())
  .get('/user/:id', req => new Response(req.params.id))
  .post('/user', () => new Response());
