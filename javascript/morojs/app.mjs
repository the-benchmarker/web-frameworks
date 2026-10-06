import { availableParallelism } from 'node:os';
import { createApp, param } from '@morojs/moro';

process.env.LOG_LEVEL = 'error';
process.env.NODE_ENV = 'production';

const app = await createApp({
  server: {
    port: 3000,
    host: '0.0.0.0',
    engine: 'moro', // Moro's native engine (@morojs/engine)
    requestTracking: { enabled: false }, // no other entry tracks requests
    requestLogging: { enabled: false }, // no other entry logs requests
    errorBoundary: { enabled: false },
  },
  performance: {
    clustering: {
      enabled: true,
      // One worker per core the container may actually run on. Moro's 'auto'
      // counts os.cpus(), which is the whole host even under --cpuset-cpus;
      // availableParallelism() honours the affinity mask, the same source the
      // other Node entries' cluster.mjs use.
      workers: availableParallelism(),
    },
  },
  logger: { level: 'warn' },
});

// A literal body in place of a handler is answered inside @morojs/engine
// without entering JS, the same way elysia-bun's literal handlers are served
// by Bun's static routes. param('id') is the same for a path parameter: the
// engine echoes the segment as the body, as res.end(req.params.id) would.
app.get('/').handler('');

app.get('/user/:id').handler(param('id'));

app.post('/user').handler('');

app.listen();
