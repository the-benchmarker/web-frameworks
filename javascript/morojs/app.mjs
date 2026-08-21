import { createApp } from '@morojs/moro';

// Set minimal logging via environment variables
process.env.LOG_LEVEL = 'error';
process.env.NODE_ENV = 'production';

// Create app with optimized config for benchmarking (clustering ENABLED for performance)
const app = await createApp({
  server: {
    port: 3000, // Default benchmark port (can be overridden by PORT env var)
    host: '0.0.0.0', // Default benchmark host (can be overridden by HOST env var)
    engine: 'moro', // force Moro's native engine
    requestTracking: {
      enabled: false, // Disable for fair comparison
    },
    errorBoundary: {
      enabled: false, // Disable for fair comparison
    },
  },
  // Minimal middleware for fair comparison
  performance: {
    clustering: {
      enabled: false, // unleash the power of clustering to really see the power
      workers: 'auto'
    },
  },

  // Minimal logging for benchmarks
  logger: {
    level: 'warn'  // This will now work correctly without env var override
  }
});

app.get('/').handler((_, res) => {
  res.send('');
});

app.get('/user/:id').handler(({params}, res) => {
  res.send(params.id);
});

app.post('/user').handler((_, res) => {
  res.send('');
});

app.listen(() => {
  const config = app.getConfig();
  console.log(`🚀 MoroJS running on port ${config.server.port}`);
});
