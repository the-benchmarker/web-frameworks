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
);

Future<void> runServer([Object? _]) async {
  final runtime = await serve(
    Server(fetch: app.fetch),
    host: '0.0.0.0',
    port: 3000,
    shared: true,
  );

  await runtime.closed;
}

Future<void> main() async {
  // Run cluster servers.
  for (int i = Platform.numberOfProcessors - 1; i > 0; i--) {
    await Isolate.spawn(runServer, null);
  }

  await runServer();
}
