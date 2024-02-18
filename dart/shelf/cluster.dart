import 'dart:io';
import 'dart:isolate';

import 'server.dart';

void main() async {
  for (var i = 0; i < Platform.numberOfProcessors - 1; i++) {
    Isolate.spawn((_) => startServer(), null);
  }

  // last server running in main isolate
  // to keep the main isolate running
  await startServer();
}
