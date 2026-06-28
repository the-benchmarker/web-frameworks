import 'dart:io';
import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as shelf_io;
import 'package:shelf_router/shelf_router.dart';
import 'package:shelf_static/shelf_static.dart';

import 'cluster.dart';

void main() {
  scale(startServer);
}

void startServer() async {
  // Create router with optimized pipeline
  final app = Router();

  // Add middleware for better performance
  final pipeline = const Pipeline()
    .addMiddleware(logRequests())
    .addMiddleware(_compressMiddleware)
    .addHandler(app);

  app.get('/', (Request request) => Response.ok(''));

  app.post('/user', (Request request) => Response.ok(''));

  app.get('/user/<user>', (Request request, String user) => Response.ok(user));

  // Configure server with optimized settings
  final server = await shelf_io.serve(
    pipeline,
    '0.0.0.0',
    3000,
    shared: true,
    securityContext: null, // Allow HTTP/1.1 without SSL for benchmarking
  );
  
  // Configure for production
  server.autoCompress = true;
  server.connectionTimeout = const Duration(seconds: 30);
  
  print('Server running on ${server.address.host}:${server.port}');
}

/// Simple compression middleware for text responses
Middleware _compressMiddleware(Handler innerHandler) {
  return (Request request) async {
    final response = await innerHandler(request);
    
    // Only compress text-based responses
    if (response.read() is String && response.mimeType?.startsWith('text/') == true) {
      return response.change(context: {'compressed': true});
    }
    
    return response;
  };
}
