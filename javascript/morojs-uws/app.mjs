import { createApp } from '@morojs/moro';

// Set minimal logging via environment variables
process.env.LOG_LEVEL = 'error';
process.env.NODE_ENV = 'production';

// Create app with optimized config for benchmarking (clustering ENABLED for performance)
const app = createApp({
  server: {
    port: 3000,        // Default benchmark port (can be overridden by PORT env var)
    host: '0.0.0.0',  // Default benchmark host (can be overridden by HOST env var)
    useUWebSockets: true,
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
          enabled: true, // unleash the power of clustering to really see the power
          workers: 'auto'
      },
  },

  // Minimal logging for benchmarks
  logger: {
      level: 'warn'  // This will now work correctly without env var override
  }
});

app.get('/', function (req, res) {
  res.send('');
});

app.get('/user/:id', function (req, res) {
  res.send(req.params.id);
});

app.post('/user', function (req, res) {
  res.send('');
});

app.listen(3000, () => {
  console.log('Server is running on port 3000');
});
