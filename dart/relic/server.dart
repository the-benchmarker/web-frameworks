import 'dart:io';

import 'package:relic/relic.dart';

Future<void> main() async {
  final app = RelicApp()
    ..get('/', (_) => Response.ok())
    ..post('/user', (_) => Response.ok())
    ..get('/user/:user', (Request request) {
      final user = request.rawPathParameters[#user]!;
      return Response.ok(body: Body.fromString(user));
    });

  await app.serve(
    address: InternetAddress.anyIPv4,
    port: 3000,
    noOfIsolates: Platform.numberOfProcessors,
  );
}
