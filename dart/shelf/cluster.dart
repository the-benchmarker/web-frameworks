import 'dart:io';
import 'dart:isolate';

/// Scales the server across multiple isolates for better CPU utilization.
/// 
/// This implementation:
/// - Spawns [numberOfProcessors - 1] isolates to handle requests
/// - Runs the main task in the current isolate
/// - Uses errorsAreFatal: false to prevent isolate crashes from bringing down the app
/// - Provides better error handling and resource cleanup
void scale(void Function() task) async {
  final processorCount = Platform.numberOfProcessors;
  final isolates = <Isolate>[];
  final errors = <Object>[];
  
  // Spawn worker isolates
  for (var i = 0; i < processorCount - 1; i++) {
    try {
      final isolate = await Isolate.spawn(
        (_) => task(),
        null,
        errorsAreFatal: false,
        onExit: (isolate) {
          // Log isolate exit but keep running
          stderr.writeln('Isolate ${isolate.hashCode} exited');
        },
        onError: (isolate, error, stackTrace) {
          errors.add(error);
          stderr.writeln('Isolate error: $error\n$stackTrace');
        },
      );
      isolates.add(isolate);
    } catch (e) {
      stderr.writeln('Failed to spawn isolate: $e');
      errors.add(e);
    }
  }

  // Run in main isolate
  try {
    task();
  } catch (e) {
    stderr.writeln('Main isolate error: $e');
    errors.add(e);
  }
  
  // Wait for all isolates to complete (though they should run indefinitely)
  for (final isolate in isolates) {
    isolate.kill(priority: Isolate.immediate);
  }
  
  // If there were errors, exit with error code
  if (errors.isNotEmpty) {
    exitCode = 1;
  }
}
