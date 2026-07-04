import { createApp, reply } from 'brahma-firelight';

const app = createApp();

// GET
app.get('/', (req) => {
  return reply.text('');
});

app.get('/user/:id', (req) => {
  return reply.text(req.params.id);
});

// POST
app.post('/user', (req) => {
  return reply.text('');
});

app.listen('0.0.0.0:3000');
