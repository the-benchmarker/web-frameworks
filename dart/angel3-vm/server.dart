import 'dart:io';
import 'dart:isolate';

import 'package:angel3_framework/angel3_framework.dart';
import 'package:angel3_framework/http.dart';
import 'package:logging/logging.dart';

Future<void> runServer([Object? _]) async {
  // A detached logger has no listeners, so nothing is printed.
  final app = Angel(
    environment: const AngelEnvironment('production'),
    logger: Logger.detached('angel'),
  );

  app.get('/', (req, res) {});

  app.get('/user/:id', (req, res) {
    res.write(req.params['id']);
  });

  app.post('/user', (req, res) {});

  // `startShared` binds with `shared: true`, one server per isolate.
  await AngelHttp.custom(
    app,
    startShared,
    useZone: false,
  ).startServer(InternetAddress.anyIPv4, 3000);
}

Future<void> main() async {
  for (int i = Platform.numberOfProcessors - 1; i > 0; i--) {
    await Isolate.spawn(runServer, null);
  }

  await runServer();
}
