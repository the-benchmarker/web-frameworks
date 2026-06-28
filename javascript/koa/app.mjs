import Koa from 'koa';
import Router from 'koa-router';

/**
 * Koa Benchmark Server
 * 
 * Production-grade benchmark server implementation using Koa framework.
 * Follows Node.js best practices including:
 * - Disabled debug logging
 * - Performance optimization
 * - Error handling
 * - Security best practices
 * - Environment variable support
 */

// Production Configuration
const PORT = parseInt(process.env.PORT || '3000', 10);
const HOST = process.env.HOST || '0.0.0.0';
const NODE_ENV = process.env.NODE_ENV || 'production';

const app = new Koa();
const router = new Router();

// Performance Optimizations
app.proxy = false; // Only enable if behind reverse proxy
app.env = NODE_ENV;

// Security: Remove X-Powered-By header
app.on('response', function (ctx) {
  ctx.response.set('X-Powered-By', '');
});

// Error handling middleware
app.on('error', (err, ctx) => {
  // In production, only log errors to stderr
  if (NODE_ENV === 'production') {
    process.stderr.write(`[${new Date().toISOString()}] ERROR: ${err.message}\n`);
  }
  ctx.status = err.status || 500;
  ctx.type = 'text/plain';
  ctx.body = '';
});

/**
 * Root endpoint handler
 * Optimized for minimal latency and maximum throughput
 * @route GET /
 * @returns {string} Empty response for benchmarking
 */
router.get('/', (ctx, next) => {
  ctx.type = 'text/plain';
  ctx.body = '';
});

/**
 * Get user by ID endpoint
 * Optimized endpoint that returns the user ID as plain text
 * @route GET /user/:id
 * @param {string} id - User identifier from path
 * @returns {string} User ID as plain text
 */
router.get('/user/:id', (ctx, next) => {
  ctx.type = 'text/plain';
  ctx.body = ctx.params.id;
});

/**
 * Create user endpoint
 * Optimized POST endpoint for creating users
 * @route POST /user
 * @returns {string} Empty response for benchmarking
 */
router.post('/user', (ctx, next) => {
  ctx.type = 'text/plain';
  ctx.body = '';
});

/**
 * Health check endpoint for monitoring
 * Production health check endpoint used by monitoring systems
 * @route GET /health
 * @returns {string} Health status
 */
router.get('/health', (ctx, next) => {
  ctx.type = 'text/plain';
  ctx.body = 'OK';
});

// Apply routes
app.use(router.routes());
app.use(router.allowedMethods({
  throw: true,
  methodNotAllowed: () => 'Method Not Allowed',
  notImplemented: () => 'Not Implemented'
}));

// Server instance for graceful shutdown
const server = app.listen(PORT, HOST, () => {
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

export default app;
