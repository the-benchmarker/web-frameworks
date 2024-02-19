import 'dart:io';
import 'dart:isolate';

void scale(void Function() task) async {
  for (var i = 0; i < Platform.numberOfProcessors - 1; i++) {
    Isolate.spawn((_) => task(), null);
  }

  // last server running in main isolate
  // to keep the main isolate running
  task();
}
