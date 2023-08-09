import { Router, macro } from '@stricjs/router';

export default new Router({ 
  base: 'http://localhost:3000',
  parsePath: false
})
  .get("/", macro(() => new Response()))
  // @ts-ignore A bit hacky here
  .get("/user/:id", macro(r => new Response(r.params.id)))
  .post("/user", macro(() => new Response()));
