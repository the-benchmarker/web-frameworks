import 'dart:io';
import 'dart:isolate';

import 'package:spry/spry.dart';
import 'package:spry/io.dart';

Future<void> runServer([_]) async {
  final app = createSpry();

  app.get('/', (event) {});
  app.post('/user', (event) {});
  app.get('/user/:name', (event) {
    final params = useParams(event);

    return params['name'];
  });

  final handler = toIOHandler(app);
  final server = await HttpServer.bind('0.0.0.0', 3000, shared: true);

  server.listen(handler);
}

Future<void> main() async {
  // Run the server in the main isolate
  await runServer();
  print('Main server running...');

  // Create a cluster of servers
  final cluster = List<Future<Isolate>>.generate(
    Platform.numberOfProcessors - 1,
    (_) => Isolate.spawn(runServer, null),
  );

  // Wait for the cluster to be ready
  await Future.wait(cluster);
  print('Cluster of servers(${cluster.length}) running...');
}
