/**
 * Express Benchmark Server
 * 
 * A benchmark server implementation using Express.js framework.
 * Follows Node.js best practices including proper error handling,
 * middleware configuration, and logging.
 */

import express from 'express';
import logger from 'morgan';
import 'express-async-errors';

// Create Express application
const app = express();

// Configure Express for benchmarking
app.set('etag', false);
app.set('x-powered-by', false);
app.set('trust proxy', true);

// Configure request body parsing
app.use(express.json({ limit: '16mb' }));
app.use(express.urlencoded({ extended: true, limit: '16mb' }));

// Configure logging for development/production
if (process.env.NODE_ENV === 'development') {
  app.use(logger('dev'));
} else {
  app.use(logger('combined'));
}

// Request logging middleware
app.use((req, res, next) => {
  const start = Date.now();
  res.on('finish', () => {
    const duration = Date.now() - start;
    console.debug(`${req.method} ${req.originalUrl} ${res.statusCode} - ${duration}ms`);
  });
  next();
});

/**
 * Root endpoint handler
 * @route GET /
 * @returns {string} Empty response for benchmarking
 */
app.get('/', (req, res) => {
  res.status(200).set('Content-Type', 'text/plain').send('');
});

/**
 * Get user by ID endpoint
 * @route GET /user/:id
 * @param {string} id - User identifier
 * @returns {string} User ID as plain text
 */
app.get('/user/:id', (req, res) => {
  const { id } = req.params;
  res.status(200).set('Content-Type', 'text/plain').send(id);
});

/**
 * Create user endpoint
 * @route POST /user
 * @returns {string} Empty response for benchmarking
 */
app.post('/user', (req, res) => {
  res.status(200).set('Content-Type', 'text/plain').send('');
});

/**
 * Health check endpoint for monitoring
 * @route GET /health
 * @returns {string} Health status
 */
app.get('/health', (req, res) => {
  res.status(200).set('Content-Type', 'text/plain').send('OK');
});

// Error handling middleware (must be last)
app.use((err, req, res, next) => {
  console.error('Unhandled error:', err);
  res.status(err.status || 500);
  res.set('Content-Type', 'text/plain');
  
  // For benchmarking, return empty response on error
  if (process.env.NODE_ENV === 'production') {
    res.send('');
  } else {
    res.send(err.message || 'Internal Server Error');
  }
});

// 404 handler
app.use((req, res) => {
  res.status(404).set('Content-Type', 'text/plain').send('Not Found');
});

export default app;
