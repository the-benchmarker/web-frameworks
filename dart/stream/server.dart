import 'dart:io';
import 'dart:isolate';

import 'package:stream/stream.dart';

final uriMapping = <String, dynamic>{
  'get:/': (HttpConnect connect) {},
  'get:/user/(?<id>[^/]+)': (HttpConnect connect) {
    connect.response.write(DefaultRouter.getNamedGroup(connect, 'id'));
  },
  'post:/user': (HttpConnect connect) {},
};

Future<void> runServer([Object? _]) async {
  await StreamServer(uriMapping: uriMapping).start(
    address: InternetAddress.anyIPv4,
    port: 3000,
    shared: true,
  );
}

Future<void> main() async {
  // One server per core; the isolates share the listening socket.
  for (int i = Platform.numberOfProcessors - 1; i > 0; i--) {
    await Isolate.spawn(runServer, null);
  }

  await runServer();
}
