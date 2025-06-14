import 'dart:io';
import 'dart:isolate';

import 'package:spry/spry.dart';

Future<void> runServer([_]) async {
  final app = createSpry();

  app.get('/', (_) {});
  app.post('/user', (_) {});
  app.get('/user/:name', (event) => event.params['name']);

  final server = app.serve(hostname: '0.0.0.0', port: 3000, reusePort: true);
  await server.ready();
}

Future<void> main() async {
  // Run main server.
  await runServer();

  // Run cluster servers.
  for (int i = Platform.numberOfProcessors - 1; i > 0; i--) {
    await Isolate.spawn(runServer, null);
  }
}
