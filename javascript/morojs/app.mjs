import { createApp } from '@morojs/moro';

// Set minimal logging via environment variables
process.env.LOG_LEVEL = 'error';
process.env.NODE_ENV = 'production';

// Create app with optimized config for benchmarking (clustering ENABLED for performance)
const app = createApp({
  performance: {
    clustering: {
      enabled: true, // Enable clustering for performance
      workers: 'auto' 
    },
    compression: {
      enabled: false  // Disable compression for fair comparison
    },
    circuitBreaker: {
      enabled: false  // Disable circuit breaker overhead
    }
  },
  cors: false,           // No CORS processing
  helmet: false,         // No security headers
  compression: false,    // No compression
  // Disable some modules for maximum performance
  modules: {
    cache: {
      enabled: true
    },
    rateLimit: {
      enabled: false
    },
    validation: {
      enabled: false
    }
  },
  logger: {
    level: 'error'  // Only log errors, no debug/info overhead
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
