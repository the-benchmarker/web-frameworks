import { Spear } from 'tspace-spear';
import uWS from 'uWebSockets.js';

// Use uWebSockets.js as the HTTP engine.
const app = new Spear({
  adapter: uWS
});

app.get('/', ({ res }) => res.end(''));
app.get('/user/:id', ({ res, params }) => res.end(`${params.id}`));
app.post('/user', ({ res }) => res.end(''));

app.listen(3000 , ({ port }) => {
  console.info(`Worker PID ${process.pid} is listening at http://localhost:${port}`);
});

export default app;
