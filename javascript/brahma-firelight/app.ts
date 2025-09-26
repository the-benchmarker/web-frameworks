import { createApp } from 'brahma-firelight';

const app = createApp();

// GET
app.get('/', (req, res) => {
  res.text('');
});

app.get('/user/:id', (req, res) => {
  res.text(req.params.id);
});

// POST
app.post('/user', (req, res) => {
  res.text('');
});

app.listen('0.0.0.0', 3000);
