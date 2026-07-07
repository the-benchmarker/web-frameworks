/**
 * Fastify Benchmark Server
 * 
 * Production-grade benchmark server implementation using Fastify framework.
 * Follows Node.js best practices including:
 * - Disabled debug logging
 * - Performance optimization
 * - Error handling
 * - Security best practices
 * - Environment variable support
 */

import fastify from 'fastify';

// Production Configuration
const PORT = parseInt(process.env.PORT || '3000', 10);
const HOST = process.env.HOST || '0.0.0.0';
const NODE_ENV = process.env.NODE_ENV || 'production';
const LOG_LEVEL = process.env.LOG_LEVEL || 'warn';

// Workers can share any TCP connection
const app = fastify({
  // Disable logging in production for maximum performance
  logger: LOG_LEVEL === 'warn' || LOG_LEVEL === 'error' ? false : {
    level: LOG_LEVEL,
    transport: NODE_ENV !== 'production' ? {
      target: 'pino-pretty',
      options: {
        translateTime: 'SYS:yyyy-mm-dd HH:MM:ss',
        ignore: 'pid,hostname',
      },
    } : undefined,
  },
  disableRequestLogging: true, // Disable request logging for benchmarking
  bodyLimit: 10 * 1024 * 1024, // 10 MB
  maxParamLength: 100,
  connectionTimeout: 30000, // 30 seconds
  keepAliveTimeout: 0, // Disable for benchmarking
  requestTimeout: 30000, // 30 seconds
  // Security settings
  disableRequestLogging: true,
  exposeHeadRoutes: false,
  return503OnClosing: true,
});

/**
 * Custom content type parser for form data
 * @param {object} req - Fastify request object
 * @param {Buffer} body - Request body
 * @param {function} done - Callback function
 */
app.addContentTypeParser('application/x-www-form-urlencoded', function (req, body, done) {
  // The incoming request in the benchmark is empty anyway
  done();
});

/**
 * Root endpoint handler
 * Optimized for minimal latency and maximum throughput
 * @route GET /
 * @returns {string} Empty response for benchmarking
 */
app.get('/', {
  schema: {
    description: 'Root endpoint for benchmarking',
    response: {
      200: { type: 'string' },
    },
  },
}, async function (request, reply) {
  reply.header('Content-Type', 'text/plain');
  reply.send('');
});

/**
 * Get user by ID endpoint
 * Optimized endpoint that returns the user ID as plain text
 * @route GET /user/:id
 * @param {string} id - User identifier from path
 * @returns {string} User ID as plain text
 */
app.get('/user/:id', {
  schema: {
    description: 'Retrieve user information by ID',
    params: {
      type: 'object',
      properties: {
        id: { type: 'string' },
      },
    },
    response: {
      200: { type: 'string' },
    },
  },
}, async function (request, reply) {
  const { id } = request.params;
  reply.header('Content-Type', 'text/plain');
  reply.send(id);
});

/**
 * Create user endpoint
 * Optimized POST endpoint for creating users
 * @route POST /user
 * @returns {string} Empty response for benchmarking
 */
app.post('/user', {
  schema: {
    description: 'Create a new user',
    response: {
      200: { type: 'string' },
    },
  },
}, async function (request, reply) {
  reply.header('Content-Type', 'text/plain');
  reply.send('');
});

/**
 * Health check endpoint for monitoring
 * Production health check endpoint used by monitoring systems
 * @route GET /health
 * @returns {string} Health status
 */
app.get('/health', {
  schema: {
    description: 'Health check endpoint for monitoring',
    response: {
      200: { type: 'string' },
    },
  },
}, async function (request, reply) {
  reply.header('Content-Type', 'text/plain');
  reply.send('OK');
});

// Global error handler
app.setErrorHandler(function (error, request, reply) {
  // In production, only log errors to stderr
  if (NODE_ENV === 'production') {
    process.stderr.write(`[${new Date().toISOString()}] ERROR: ${error.message}\n`);
  }
  reply.header('Content-Type', 'text/plain');
  reply.code(500).send('');
});

// 404 handler
app.setNotFoundHandler(function (request, reply) {
  reply.header('Content-Type', 'text/plain');
  reply.code(404).send('Not Found');
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  if (NODE_ENV !== 'production') {
    console.log('SIGTERM received. Shutting down gracefully...');
  }
  await app.close();
  process.exit(0);
});

process.on('SIGINT', async () => {
  if (NODE_ENV !== 'production') {
    console.log('SIGINT received. Shutting down gracefully...');
  }
  await app.close();
  process.exit(0);
});

// Start server
app.listen({ port: PORT, host: HOST }, function (err, address) {
  if (err) {
    if (NODE_ENV === 'production') {
      process.stderr.write(`[${new Date().toISOString()}] ERROR: ${err.message}\n`);
    } else {
      console.error('Failed to start server:', err);
    }
    process.exit(1);
  }
  
  // Only log startup in non-production environments
  if (NODE_ENV !== 'production') {
    console.log(`Worker PID ${process.pid} is listening at ${address}`);
  }
});

export default app;
