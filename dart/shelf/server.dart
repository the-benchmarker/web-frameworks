import 'dart:io';
import 'dart:isolate';

import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as shelf_io;
import 'package:shelf_router/shelf_router.dart';

void main() async {
  for (var i = 0; i < Platform.numberOfProcessors - 1; i++) {
    Isolate.spawn((_) => startServer(), null);
  }

  // last server running in main isolate
  // to keep the main isolate running
  await startServer();
}

Future<void> startServer() async {
  var app = Router();

  app.get('/', (Request request) => Response.ok(''));

  app.post('/user', (Request request) => Response.ok(''));

  app.get('/user/<user>', (Request request, String user) => Response.ok(user));

  await shelf_io.serve(app, '0.0.0.0', 3000, shared: true);
}
