import { createApp, type Context, type App } from 'brahma-firelight';

const app: App = createApp();

// GET
app.get('/', (ctx: Context) => {
  return ctx.text('');
});

app.get('/user/:id', (ctx: Context) => {
  return ctx.text(ctx.req.params?.id);
});

// POST
app.post('/user', (ctx: Context) => {
  return ctx.text('');
});

app.listen(3000, '0.0.0.0', true);
