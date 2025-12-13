import { Ignisia } from 'ignisia';

new Ignisia()
  .get('/', (c) => c.text(''))
  .get('/user/:id', (c) => c.text(c.req.param('id')))
  .post('/user', (c) => c.text(''))
  .listen({
    port: 3000,
    reusePort: true,
  });
