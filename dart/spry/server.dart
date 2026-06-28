import 'dart:io';
import 'dart:isolate';

import 'package:spry/osrv.dart' show Server;
import 'package:spry/osrv/dart.dart' show serve;
import 'package:spry/spry.dart' show HttpMethod, Response, Spry;

final app = Spry(
  routes: {
    '/': {HttpMethod.get: (_) => Response(null)},
    '/user': {HttpMethod.post: (_) => Response(null)},
    '/user/:name': {
      HttpMethod.get: (event) => Response(event.params.required('name')),
    },
  },
  // Configure Spry for production
  configuration: SpryConfiguration(
    compressResponse: true,
    maxRequestBodySize: 1024 * 1024, // 1MB
    requestTimeout: const Duration(seconds: 30),
  ),
);

Future<void> runServer([Object? _]) async {
  final runtime = await serve(
    Server(
      fetch: app.fetch,
      // Optimize server settings
      maxConnections: 1000,
      connectionTimeout: const Duration(seconds: 30),
    ),
    host: '0.0.0.0',
    port: 3000,
    shared: true,
  );

  await runtime.closed;
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
