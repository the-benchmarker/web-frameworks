import cluster, { type Worker } from 'node:cluster';

/**
 * Bun Benchmark Server Cluster
 * 
 * Production-grade benchmark server implementation using Bun runtime.
 * Follows best practices including:
 * - Disabled debug logging
 * - Performance optimization
 * - Error handling
 * - Environment variable support
 * - Multi-core support
 */

// Production Configuration
const PORT = parseInt(process.env.PORT || '3000', 10);
const HOST = process.env.HOST || '0.0.0.0';
const NODE_ENV = process.env.NODE_ENV || 'production';

function startServer() {
  const server = Bun.serve({
    hostname: HOST,
    port: PORT,
    reusePort: true,
    development: NODE_ENV !== 'production',
    // Performance settings
    maxHeaderSize: 8192, // 8KB
    maxRequestBodySize: 10 * 1024 * 1024, // 10MB
    
    routes: {
      /**
       * Root endpoint handler
       * Optimized for minimal latency and maximum throughput
       * @route GET /
       * @returns {Response} Empty response with 204 status
       */
      '/': () => new Response(null, { 
        status: 204,
        headers: { 'Content-Type': 'text/plain' }
      }),
      
      /**
       * Create user endpoint
       * Optimized POST endpoint for creating users
       * @route POST /user
       * @returns {Response} Empty response with 204 status
       */
      '/user': () => new Response(null, { 
        status: 204,
        headers: { 'Content-Type': 'text/plain' }
      }),
      
      /**
       * Get user by ID endpoint
       * Optimized endpoint that returns the user ID as plain text
       * @route GET /user/:id
       * @param {string} id - User identifier from path
       * @returns {Response} User ID as plain text
       */
      '/user/:id': ({ params }) => new Response(params.id, { 
        status: 200,
        headers: { 'Content-Type': 'text/plain' }
      }),
      
      /**
       * Health check endpoint for monitoring
       * Production health check endpoint used by monitoring systems
       * @route GET /health
       * @returns {Response} Health status
       */
      '/health': () => new Response('OK', { 
        status: 200,
        headers: { 'Content-Type': 'text/plain' }
      }),
    },
  });

  // Only log startup in non-production environments
  if (NODE_ENV !== 'production') {
    console.debug(`Worker PID: ${process.pid} listening on ${server.url}`);
  }
}

function forkWorkers() {
  // Only log cluster info in non-production
  if (NODE_ENV !== 'production') {
    console.log(`Primary PID: ${process.pid}`);
  }
  
  for (let i = 0; i < navigator.hardwareConcurrency; i++) cluster.fork();

  cluster.on('exit', (worker, code, signal) => {
    if (NODE_ENV !== 'production') {
      console.error(`Worker PID: ${worker.process.pid} died with code ${code} and signal ${signal}`);
    } else {
      // In production, log to stderr only
      process.stderr.write(`[${new Date().toISOString()}] ERROR: Worker ${worker.process.pid} died with code ${code}\n`);
    }
  });
}

// Graceful shutdown for primary process
if (cluster.isPrimary) {
  process.on('SIGTERM', () => {
    if (NODE_ENV !== 'production') {
      console.log('Primary received SIGTERM. Disconnecting workers...');
    }
    cluster.disconnect(() => process.exit(0));
  });

  process.on('SIGINT', () => {
    if (NODE_ENV !== 'production') {
      console.log('Primary received SIGINT. Disconnecting workers...');
    }
    cluster.disconnect(() => process.exit(0));
  });
  
  forkWorkers();
} else {
  startServer();
}
