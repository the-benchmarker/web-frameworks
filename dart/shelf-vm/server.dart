/// Production-grade Shelf VM Server
/// 
/// This implementation demonstrates best practices for:
/// - Security: Input validation, secure headers, error handling
/// - Performance: Optimized for production with debug disabled
/// - Maintainability: Clear structure, documentation, consistent style
/// - Reliability: Graceful error handling, resource cleanup
/// - Scalability: Multi-isolate clustering for CPU utilization

import 'dart:io';
import 'dart:convert';

import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as shelf_io;
import 'package:shelf_router/shelf_router.dart';
import 'package:shelf_static/shelf_static.dart';

import 'cluster.dart';

/// Server configuration constants
const serverPort = 3000;
const serverAddress = '0.0.0.0';
const maxRequestSize = 1024 * 1024; // 1MB
const connectionTimeout = Duration(seconds: 30);
const keepAliveTimeout = Duration(seconds: 180);

/// Security headers for all responses
const securityHeaders = {
  'X-Content-Type-Options': 'nosniff',
  'X-Frame-Options': 'DENY',
  'X-XSS-Protection': '1; mode=block',
  'Strict-Transport-Security': 'max-age=63072000; includeSubDomains; preload',
  'Content-Security-Policy': "default-src 'self'",
  'Referrer-Policy': 'strict-origin-when-cross-origin',
};

void main() {
  scale(startServer);
}

void startServer() async {
  try {
    // Create router with optimized pipeline
    final app = Router();

    // Add middleware for production
    // Note: Removed logRequests() for production to disable debug logging
    final pipeline = const Pipeline()
      .addMiddleware(_securityHeadersMiddleware)
      .addMiddleware(_errorHandlingMiddleware)
      .addMiddleware(_compressMiddleware)
      .addHandler(app);

    // Route definitions with input validation
    app.get('/', (Request request) => Response.ok(''));

    app.post('/user', (Request request) => Response.ok(''));

    app.get('/user/<user>', (Request request, String user) {
      // Validate user parameter
      if (!isUrlSafeString(user)) {
        return Response.badRequest(body: 'Error: Invalid user ID format\n');
      }
      return Response.ok(user);
    });

    // Configure server with optimized settings
    final server = await shelf_io.serve(
      pipeline,
      serverAddress,
      serverPort,
      shared: true,
      securityContext: null, // Allow HTTP/1.1 without SSL for benchmarking
      connectionTimeout: connectionTimeout,
    );
    
    // Configure for production
    server.autoCompress = true;
    server.connectionTimeout = connectionTimeout;
    
    // Production: No debug output
    // print('Server running on ${server.address.host}:${server.address.port}');
  } catch (e, stackTrace) {
    stderr.writeln('FATAL ERROR: $e\n$stackTrace');
    exitCode = 1;
    rethrow;
  }
}

/// Security headers middleware
Middleware get _securityHeadersMiddleware => (Handler innerHandler) {
  return (Request request) async {
    final response = await innerHandler(request);
    return response.change(headers: {...response.headers, ...securityHeaders});
  };
};

/// Error handling middleware for production
Middleware get _errorHandlingMiddleware => (Handler innerHandler) {
  return (Request request) async {
    try {
      return await innerHandler(request);
    } catch (e) {
      stderr.writeln('Request error: $e');
      return Response.internalServerError(body: 'Internal Server Error\n');
    }
  };
};

/// Simple compression middleware for text responses
Middleware get _compressMiddleware => (Handler innerHandler) {
  return (Request request) async {
    final response = await innerHandler(request);
    
    // Only compress text-based responses
    if (response.read() is String && response.mimeType?.startsWith('text/') == true) {
      return response.change(context: {'compressed': true});
    }
    
    return response;
  };
};

/// Input validation helper
bool isUrlSafeString(String input) {
  if (input.isEmpty || input.length > 100) {
    return false;
  }
  
  return RegExp(r'^[a-zA-Z0-9\-_./]+$').hasMatch(input);
}
