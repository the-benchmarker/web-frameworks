/// Production-grade Spry VM Server
/// 
/// This implementation demonstrates best practices for:
/// - Security: Input validation, secure headers, error handling
/// - Performance: Optimized for production with debug disabled
/// - Maintainability: Clear structure, documentation, consistent style
/// - Reliability: Graceful error handling, resource cleanup
/// - Scalability: Multi-isolate clustering for CPU utilization

import 'dart:io';
import 'dart:isolate';
import 'dart:convert';

import 'package:spry/osrv.dart' show Server;
import 'package:spry/osrv/dart.dart' show serve;
import 'package:spry/spry.dart' show HttpMethod, Response, Spry, RequestEvent;

/// Server configuration constants
const serverPort = 3000;
const serverAddress = '0.0.0.0';
const maxRequestSize = 1024 * 1024; // 1MB
const maxConnections = 1000;
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

final app = Spry(
  routes: {
    '/': {HttpMethod.get: _handleRoot},
    '/user': {HttpMethod.post: _handlePostUser},
    '/user/:name': {HttpMethod.get: _handleGetUser},
  },
  // Configure Spry for production
  configuration: SpryConfiguration(
    compressResponse: true,
    maxRequestBodySize: maxRequestSize,
    requestTimeout: connectionTimeout,
    // Disable debug mode
    debug: false,
  ),
);

/// Handle GET /
Response _handleRoot(RequestEvent event) {
  return Response(
    null,
    headers: {...securityHeaders, 'Content-Type': 'text/plain; charset=utf-8'},
  );
}

/// Handle POST /user
Response _handlePostUser(RequestEvent event) {
  return Response(
    null,
    statusCode: HttpStatus.created,
    headers: {...securityHeaders, 'Content-Type': 'text/plain; charset=utf-8'},
  );
}

/// Handle GET /user/:name with input validation
Response _handleGetUser(RequestEvent event) {
  final name = event.params.required('name');
  
  // Validate user parameter
  if (!isUrlSafeString(name)) {
    return Response(
      'Error: Invalid user ID format\n',
      statusCode: HttpStatus.badRequest,
      headers: {...securityHeaders, 'Content-Type': 'text/plain; charset=utf-8'},
    );
  }
  
  return Response(
    name,
    headers: {...securityHeaders, 'Content-Type': 'text/plain; charset=utf-8'},
  );
}

Future<void> runServer([Object? _]) async {
  try {
    final runtime = await serve(
      Server(
        fetch: app.fetch,
        // Optimize server settings
        maxConnections: maxConnections,
        connectionTimeout: connectionTimeout,
        keepAliveTimeout: keepAliveTimeout,
      ),
      host: serverAddress,
      port: serverPort,
      shared: true,
    );

    await runtime.closed;
  } catch (e, stackTrace) {
    stderr.writeln('FATAL ERROR in Spry server: $e\n$stackTrace');
    exitCode = 1;
    rethrow;
  }
}

Future<void> main() async {
  // Run cluster servers with better error handling
  final processorCount = Platform.numberOfProcessors;
  final isolates = <Isolate>[];
  
  for (int i = processorCount - 1; i > 0; i--) {
    try {
      final isolate = await Isolate.spawn(
        runServer,
        null,
        errorsAreFatal: false,
        onExit: (isolate) {
          stderr.writeln('Spry isolate ${isolate.hashCode} exited');
        },
        onError: (isolate, error, stackTrace) {
          stderr.writeln('Spry isolate error: $error\n$stackTrace');
        },
      );
      isolates.add(isolate);
    } catch (e) {
      stderr.writeln('Failed to spawn Spry isolate: $e');
    }
  }

  // Run in main isolate
  await runServer();
  
  // Cleanup isolates on shutdown (though they should run indefinitely)
  for (final isolate in isolates) {
    isolate.kill(priority: Isolate.immediate);
  }
}

/// Input validation helper
bool isUrlSafeString(String input) {
  if (input.isEmpty || input.length > 100) {
    return false;
  }
  
  return RegExp(r'^[a-zA-Z0-9\-_./]+$').hasMatch(input);
}
