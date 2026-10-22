import { Spear } from 'tspace-spear';
import uWS from 'uWebSockets.js';

// Use uWebSockets.js as the HTTP engine and enable clustering.
const app = new Spear({
  adapter: uWS,
  cluster: true,
});

app
  .get('/', () => '')
  .get('/user/:id', ({ params }) => `${params.id}`)
  .post('/user', () => '');

app.listen(3000);