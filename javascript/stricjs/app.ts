import { Router } from '@stricjs/router';  

export default new Router()
  .get('/', () => new Response(""))
  .get('/user/:id', ({ params: { id } }) => new Response(id))
  .post('/user', () => new Response(""));
