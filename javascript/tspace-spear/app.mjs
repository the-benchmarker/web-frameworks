import { Spear } from 'tspace-spear';
import uWS from 'uWebSockets.js';

// Use uWebSockets.js as the HTTP engine and enable clustering.
const app = new Spear({
  adapter: uWS
});

app.get('/', () => '');
app.get('/user/:id', ({ params }) => `${params.id}`);
app.post('/user', () => '');

app.listen(3000 , ({ port }) => {
  console.info(`Worker PID ${process.pid} is listening at http://localhost:${port}`);
});

export default app;
