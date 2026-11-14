import 'package:dox_core/dox_core.dart';

class AppRouter extends Router {
  @override
  void register() {
    Route.get('/', (DoxRequest req) => '');
    Route.get('/user/{id}', (String id) => id);
    Route.post('/user', (DoxRequest req) => '');
  }
}

Future<void> main() async {
  Dox().initialize(AppConfig(
    appKey: 'benchmarkbenchmarkbenchmark12345',
    serverPort: 3000,
    routers: <Router>[AppRouter()],
    logger: LoggerConfig(enabled: false),
  ));
  await Dox().startServer();
}
