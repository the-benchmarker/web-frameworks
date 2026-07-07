import { Hono } from 'hono';
import { serve } from '@hono/node-server';

/**
 * Hono Benchmark Server (Node.js)
 * 
 * Production-grade benchmark server implementation using Hono framework on Node.js.
 * Follows best practices including:
 * - Disabled debug logging
 * - Performance optimization
 * - Error handling
 * - Environment variable support
 */

// Production Configuration
const PORT = parseInt(process.env.PORT || '3000', 10);
const HOST = process.env.HOST || '0.0.0.0';
const NODE_ENV = process.env.NODE_ENV || 'production';

const app = new Hono();

/**
 * Global error handler
 * Handles all uncaught exceptions and returns appropriate responses
 */
app.onError((err, c) => {
  // In production, only log errors to stderr
  if (NODE_ENV === 'production') {
    process.stderr.write(`[${new Date().toISOString()}] ERROR: ${err.message}\n`);
  } else {
    console.error('Unhandled error:', err);
  }
  return c.text('', 500);
});

/**
 * 404 handler
 */
app.notFound((c) => {
  return c.text('Not Found', 404);
});

/**
 * Root endpoint handler
 * Optimized for minimal latency and maximum throughput
 * @route GET /
 * @returns {string} Empty response for benchmarking
 */
app.get('/', (c) => {
  return c.text('');
});

/**
 * Get user by ID endpoint
 * Optimized endpoint that returns the user ID as plain text
 * @route GET /user/:id
 * @param {string} id - User identifier from path
 * @returns {string} User ID as plain text
 */
app.get('/user/:id', (c) => {
  return c.text(c.req.param('id'));
});

/**
 * Create user endpoint
 * Optimized POST endpoint for creating users
 * @route POST /user
 * @returns {string} Empty response for benchmarking
 */
app.post('/user', (c) => {
  return c.text('');
});

/**
 * Health check endpoint for monitoring
 * Production health check endpoint used by monitoring systems
 * @route GET /health
 * @returns {string} Health status
 */
app.get('/health', (c) => {
  return c.text('OK');
});

// Start server
const server = serve({
  fetch: app.fetch,
  port: PORT,
  hostname: HOST
}, () => {
  // Only log startup in non-production environments
  if (NODE_ENV !== 'production') {
    console.log(`Worker PID ${process.pid} is listening at http://${HOST}:${PORT}`);
  }
});

// Graceful shutdown
process.on('SIGTERM', () => {
  if (NODE_ENV !== 'production') {
    console.log('SIGTERM received. Shutting down gracefully...');
  }
  server.close(() => {
    process.exit(0);
  });
});

process.on('SIGINT', () => {
  if (NODE_ENV !== 'production') {
    console.log('SIGINT received. Shutting down gracefully...');
  }
  server.close(() => {
    process.exit(0);
  });
});
