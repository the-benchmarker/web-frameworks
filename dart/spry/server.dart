import 'dart:io';
import 'dart:isolate';

import 'package:spry/spry.dart';

Future<void> server([_]) async {
  final app =
      await Application.create(port: 3000, address: '0.0.0.0', shared: true);

  app.get('/', (request) {});
  app.post('/user', (request) {});
  app.get('/user/:name', (request) => request.params.get('name'));

  await app.listen();
}

Future<void> main() async {
  // Run the server in the main isolate
  await server();
  print('Main server running...');

  // Create a cluster of servers
  final cluster = List<Future<Isolate>>.generate(
    Platform.numberOfProcessors - 1,
    (_) => Isolate.spawn(server, null),
  );

  // Wait for the cluster to be ready
  await Future.wait(cluster);
  print('Cluster of servers(${cluster.length}) running...');
}
