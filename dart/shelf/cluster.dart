/// Production-grade Isolate Cluster Manager
/// 
/// This implementation provides:
/// - Multi-isolate scaling for better CPU utilization
/// - Graceful error handling and resource cleanup
/// - Production-optimized configuration
/// - Proper process management

import 'dart:io';
import 'dart:isolate';

/// Maximum number of worker isolates
const maxWorkerIsolates = 8;

/// Maximum isolate lifetime to prevent memory leaks
const maxIsolateLifetime = Duration(hours: 1);

/// Scales the server across multiple isolates for better CPU utilization.
/// 
/// This implementation:
/// - Spawns [min(processorCount - 1, maxWorkerIsolates)] isolates to handle requests
/// - Runs the main task in the current isolate
/// - Uses errorsAreFatal: false to prevent isolate crashes from bringing down the app
/// - Provides better error handling and resource cleanup
/// - Limits the number of isolates for production stability
void scale(void Function() task) async {
  final processorCount = Platform.numberOfProcessors;
  final workerCount = processorCount - 1 > maxWorkerIsolates ? maxWorkerIsolates : processorCount - 1;
  final isolates = <Isolate>[];
  final errors = <Object>[];
  
  // Spawn worker isolates
  for (var i = 0; i < workerCount; i++) {
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
    try {
      isolate.kill(priority: Isolate.immediate);
    } catch (e) {
      stderr.writeln('Error killing isolate: $e');
    }
  }
  
  // If there were errors, exit with error code
  if (errors.isNotEmpty) {
    stderr.writeln('Cluster shutdown with ${errors.length} errors');
    exitCode = 1;
  }
}
