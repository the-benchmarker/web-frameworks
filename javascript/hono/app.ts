import { Hono } from 'hono';
import { serve } from 'bun';

const PORT = 3000;

const app = new Hono();

app.get('/', (c) => c.text(''));
app.get('/user/:id', (c) => c.text(c.req.param('id')));
app.post('/user', (c) => c.text(''));

serve({
  port: PORT,
  reusePort: true,
  fetch: app.fetch,
});

console.info(`Worker PID: ${process.pid} is listening at http://localhost:${3000}`);
