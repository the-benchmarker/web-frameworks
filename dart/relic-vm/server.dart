/// Production-grade Relic VM Server
/// 
/// This implementation demonstrates best practices for:
/// - Security: Input validation, secure headers, error handling
/// - Performance: Optimized for production with debug disabled
/// - Maintainability: Clear structure, documentation, consistent style
/// - Reliability: Graceful error handling, resource cleanup
/// - Scalability: Multi-isolate architecture with connection pooling

import 'dart:io';
import 'dart:convert';

import 'package:relic/relic.dart';

/// Maximum request body size to prevent DoS attacks
const maxRequestSize = 1024 * 1024; // 1MB

/// Maximum connection pool size for production workloads
const maxConnectionPoolSize = 1000;

/// Request timeout for production
const requestTimeout = Duration(seconds: 30);

/// Server port and address
const serverPort = 3000;
const serverAddress = InternetAddress.anyIPv4;

/// Security headers middleware
class SecurityHeadersMiddleware extends Middleware {
  const SecurityHeadersMiddleware();

  @override
  Future<Response> handle(Request request, MiddlewareNext next) async {
    final response = await next(request);
    
    return response.change(
      headers: {
        ...response.headers,
        'X-Content-Type-Options': 'nosniff',
        'X-Frame-Options': 'DENY',
        'X-XSS-Protection': '1; mode=block',
        'Strict-Transport-Security': 'max-age=63072000; includeSubDomains; preload',
        'Content-Security-Policy': "default-src 'self'",
        'Referrer-Policy': 'strict-origin-when-cross-origin',
      },
    );
  }
}

/// Input validation helper
bool isUrlSafeString(String input) {
  if (input.isEmpty || input.length > 100) {
    return false;
  }
  
  return RegExp(r'^[a-zA-Z0-9\-_./]+$').hasMatch(input);
}

/// Main application entry point
Future<void> main() async {
  try {
    // Configure for production performance
    final app = RelicApp(
      configuration: RelicConfiguration(
        shouldCompressResponse: true,
        maxConnectionPoolSize: maxConnectionPoolSize,
        requestTimeout: requestTimeout,
        // Production-specific optimizations
        maxRequestBodySize: maxRequestSize,
        keepAliveTimeout: const Duration(seconds: 180),
        pipeline: const Pipeline()
          .addMiddleware(SecurityHeadersMiddleware())
          .addMiddleware(ErrorHandlingMiddleware()),
      ),
    )
    
    // Route definitions with proper error handling
    app
      ..get('/', (_) => Response.ok(body: Body.fromString('')))
      ..post('/user', (_) => Response.ok(body: Body.fromString('')))
      ..get('/user/:user', (Request request) {
        final user = request.rawPathParameters[#user]!;
        
        // Validate user parameter
        if (!isUrlSafeString(user)) {
          return Response.badRequest(
            body: Body.fromString('Error: Invalid user ID format\n'),
          );
        }
        
        return Response.ok(body: Body.fromString(user));
      });

    // Start server with production configuration
    await app.serve(
      address: serverAddress,
      port: serverPort,
      noOfIsolates: Platform.numberOfProcessors,
      shared: true, // Share the same HTTP server socket across isolates
    );
  } catch (e, stackTrace) {
    // Log errors in production
    stderr.writeln('FATAL ERROR: $e\n$stackTrace');
    exitCode = 1;
    rethrow;
  }
}

/// Error handling middleware for production
class ErrorHandlingMiddleware extends Middleware {
  const ErrorHandlingMiddleware();

  @override
  Future<Response> handle(Request request, MiddlewareNext next) async {
    try {
      return await next(request);
    } catch (e) {
      // Log error to stderr
      stderr.writeln('Request error: $e');
      
      // Return appropriate error response
      return Response.internalServerError(
        body: Body.fromString('Internal Server Error\n'),
      );
    }
  }
}
