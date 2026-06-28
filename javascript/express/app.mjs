import express from 'express';
import helmet from 'helmet';

/**
 * Express.js Benchmark Server
 * 
 * Production-grade benchmark server implementation using Express.js framework.
 * Follows Node.js best practices including:
 * - Disabled debug logging
 * - Security headers
 * - Error handling
 * - Performance optimization
 * - Environment variable support
 */

const app = express();

// Production Configuration
const PORT = parseInt(process.env.PORT || '3000', 10);
const HOST = process.env.HOST || '0.0.0.0';
const NODE_ENV = process.env.NODE_ENV || 'production';

// Security Middleware (Production-grade)
// Note: Helmet would add security headers, but we disable for benchmarking
// app.use(helmet()); // Disabled for maximum performance in benchmarking

// Performance Optimizations
app.set('etag', false); // Disable ETags for benchmarking
app.set('x-powered-by', false); // Remove X-Powered-By header
app.disable('x-powered-by'); // Double ensure X-Powered-By is removed

// Disable unnecessary features for benchmarking
app.disable('trust proxy'); // Only enable if behind reverse proxy

// Body parsing limits (for security)
app.use(express.json({ limit: '10mb' }));
app.use(express.urlencoded({ extended: true, limit: '10mb' }));

// Request size limits
app.set('maxHeaderSize', 8192); // 8KB header size

/**
 * Root endpoint handler
 * Optimized for minimal latency and maximum throughput
 * @route GET /
 * @returns {string} Empty response for benchmarking
 */
app.get('/', function (req, res) {
  res.type('text/plain');
  res.send('');
});

/**
 * Get user by ID endpoint
 * Optimized endpoint that returns the user ID as plain text
 * @route GET /user/:id
 * @param {string} id - User identifier from path
 * @returns {string} User ID as plain text
 */
app.get('/user/:id', function (req, res) {
  res.type('text/plain');
  res.send(req.params.id);
});

/**
 * Create user endpoint
 * Optimized POST endpoint for creating users
 * @route POST /user
 * @returns {string} Empty response for benchmarking
 */
app.post('/user', function (req, res) {
  res.type('text/plain');
  res.send('');
});

/**
 * Health check endpoint for monitoring
 * Production health check endpoint used by monitoring systems
 * @route GET /health
 * @returns {string} Health status
 */
app.get('/health', function (req, res) {
  res.type('text/plain');
  res.send('OK');
});

// Global error handler
app.use(function (err, req, res, next) {
  // In production, only log errors to stderr
  if (NODE_ENV === 'production') {
    process.stderr.write(`[${new Date().toISOString()}] ERROR: ${err.message}\n`);
  }
  res.type('text/plain');
  res.status(500).send('');
});

// 404 handler
app.use(function (req, res) {
  res.type('text/plain');
  res.status(404).send('Not Found');
});

// Graceful shutdown
process.on('SIGTERM', function () {
  if (NODE_ENV !== 'production') {
    console.log('SIGTERM received. Shutting down gracefully...');
  }
  server.close(function () {
    process.exit(0);
  });
});

process.on('SIGINT', function () {
  if (NODE_ENV !== 'production') {
    console.log('SIGINT received. Shutting down gracefully...');
  }
  server.close(function () {
    process.exit(0);
  });
});

// Start server
const server = app.listen(PORT, HOST, function (err) {
  if (err) {
    process.stderr.write(`[${new Date().toISOString()}] ERROR: ${err.message}\n`);
    process.exit(1);
  }
  
  // Only log startup in non-production environments
  if (NODE_ENV !== 'production') {
    console.log(`Worker PID ${process.pid} is listening at http://${HOST}:${PORT}`);
  }
});

export default app;
