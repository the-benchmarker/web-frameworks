import 'dart:async';
import 'dart:io';
import 'dart:math';

import 'package:aqueduct/aqueduct.dart';

class AqueductDartChannel extends ApplicationChannel {
  @override
  Controller get entryPoint {
    final router = Router();

    router.route("/").linkFunction((request) =>
        Response.ok("")..contentType = new ContentType("text", "plain"));

    router.route("/user/:id").linkFunction((Request request) =>
        Response.ok(request.path.variables['id'])
          ..contentType = new ContentType("text", "plain"));

    router.route("/user").linkFunction((Request request) {
      if (request.method != 'POST') return Response.notFound();
      return Response.ok("")..contentType = new ContentType("text", "plain");
    });

    return router;
  }
}

Future main() async {
  final app = Application<AqueductDartChannel>()
    ..options.configurationFilePath = "config.yaml"
    ..options.port = 3000;

  final count = Platform.numberOfProcessors ~/ 2;
  await app.start(numberOfInstances: count > 0 ? count : 1);

  print("Application started on port: ${app.options.port}.");
  print("Use Ctrl-C (SIGINT) to stop running the application.");
}
