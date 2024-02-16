import { Hono } from 'https://deno.land/x/hono/mod.ts'

const app = new Hono();

app.get('/', (c) => c.text(''));
app.get('/user/:id', (c) => c.text(c.req.param('id')));
app.post('/user', (c) => c.text(''));

Deno.serve({port: 3000}, app.fetch)
