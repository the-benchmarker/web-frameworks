/**
 * Fastify Benchmark Server
 * 
 * A high-performance benchmark server implementation using Fastify framework.
 * Follows Node.js best practices including proper error handling, middleware,
 * and logging.
 */

import fastify from 'fastify';
import fastifyPlugin from 'fastify-plugin';

// Workers can share any TCP connection
const app = fastify({
  logger: {
    level: process.env.LOG_LEVEL || 'info',
    transport: {
      target: 'pino-pretty',
      options: {
        translateTime: 'SYS:yyyy-mm-dd HH:MM:ss',
        ignore: 'pid,hostname',
      },
    },
  },
  disableRequestLogging: false,
  bodyLimit: 16 * 1024 * 1024, // 16 MB
  maxParamLength: 100,
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
 * Request logging plugin
 * Adds custom request logging with timing
 */
app.register(fastifyPlugin(async function (fastify) {
  fastify.addHook('onRequest', async (request, reply) => {
    request.startTime = process.hrtime.bigint();
  });

  fastify.addHook('onResponse', async (request, reply) => {
    const duration = process.hrtime.bigint() - request.startTime;
    fastify.log.info({
      method: request.method,
      url: request.url,
      statusCode: reply.statusCode,
      duration: Number(duration) / 1e6, // Convert to ms
    }, 'request completed');
  });
}));

/**
 * Root endpoint handler
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
  app.log.debug('Root endpoint accessed');
  reply.header('Content-Type', 'text/plain');
  reply.send('');
});

/**
 * Get user by ID endpoint
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
  app.log.debug(`User endpoint accessed with ID: ${id}`);
  reply.header('Content-Type', 'text/plain');
  reply.send(id);
});

/**
 * Create user endpoint
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
  app.log.debug('Create user endpoint accessed');
  reply.header('Content-Type', 'text/plain');
  reply.send('');
});

/**
 * Health check endpoint for monitoring
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
  app.log.error('Unhandled error:', error);
  
  if (process.env.NODE_ENV === 'production') {
    reply.header('Content-Type', 'text/plain');
    reply.code(500).send('');
  } else {
    reply.code(500).send(error.message || 'Internal Server Error');
  }
});

// 404 handler
app.setNotFoundHandler(function (request, reply) {
  reply.header('Content-Type', 'text/plain');
  reply.code(404).send('Not Found');
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  app.log.info('SIGTERM received. Shutting down gracefully...');
  await app.close();
  process.exit(0);
});

process.on('SIGINT', async () => {
  app.log.info('SIGINT received. Shutting down gracefully...');
  await app.close();
  process.exit(0);
});

// Running Node.js will now share port between the workers:
const port = parseInt(process.env.PORT || '3000', 10);
const host = process.env.HOST || '0.0.0.0';

app.listen({ port, host }, function (err, address) {
  if (err) {
    app.log.error(err);
    process.exit(1);
  }
  app.log.info(`Worker PID ${process.pid} is listening at ${address}`);
});

export default app;
