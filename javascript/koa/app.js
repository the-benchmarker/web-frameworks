/**
 * Koa Benchmark Server
 * 
 * A benchmark server implementation using Koa framework.
 * Follows Node.js best practices including proper error handling,
 * middleware configuration, and logging.
 */

import Koa from 'koa';
import Router from 'koa-router';
import bodyParser from 'koa-bodyparser';
import logger from 'koa-logger';
import json from 'koa-json';

// Create Koa application
const app = new Koa();
const router = new Router();

// Configure middleware
app.use(json());
app.use(bodyParser({
  limit: '16mb',
  enableTypes: ['json', 'form'],
}));

// Configure logging based on environment
if (process.env.NODE_ENV === 'development') {
  app.use(logger());
}

// Request logging middleware
app.use(async (ctx, next) => {
  const start = Date.now();
  try {
    await next();
    const duration = Date.now() - start;
    console.debug(`${ctx.method} ${ctx.originalUrl} ${ctx.status} - ${duration}ms`);
  } catch (err) {
    const duration = Date.now() - start;
    console.error(`${ctx.method} ${ctx.originalUrl} ${ctx.status || 500} - ${duration}ms`);
    throw err;
  }
});

/**
 * Root endpoint handler
 * @route GET /
 * @returns {string} Empty response for benchmarking
 */
router.get('/', (ctx) => {
  console.debug('Root endpoint accessed');
  ctx.type = 'text/plain';
  ctx.body = '';
});

/**
 * Get user by ID endpoint
 * @route GET /user/:id
 * @param {string} id - User identifier from path
 * @returns {string} User ID as plain text
 */
router.get('/user/:id', (ctx) => {
  const { id } = ctx.params;
  console.debug(`User endpoint accessed with ID: ${id}`);
  ctx.type = 'text/plain';
  ctx.body = id;
});

/**
 * Create user endpoint
 * @route POST /user
 * @returns {string} Empty response for benchmarking
 */
router.post('/user', (ctx) => {
  console.debug('Create user endpoint accessed');
  ctx.type = 'text/plain';
  ctx.body = '';
});

/**
 * Health check endpoint for monitoring
 * @route GET /health
 * @returns {string} Health status
 */
router.get('/health', (ctx) => {
  ctx.type = 'text/plain';
  ctx.body = 'OK';
});

// Use router
app.use(router.routes());
app.use(router.allowedMethods());

// Error handling middleware (must be last)
app.on('error', (err, ctx) => {
  console.error('Unhandled error:', err);
  
  // For benchmarking, return empty response in production
  if (process.env.NODE_ENV === 'production') {
    ctx.type = 'text/plain';
    ctx.body = '';
    ctx.status = err.status || 500;
  } else {
    ctx.type = 'text/plain';
    ctx.body = err.message || 'Internal Server Error';
    ctx.status = err.status || 500;
  }
});

export { app, router };
