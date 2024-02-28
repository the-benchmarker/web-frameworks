import 'package:spry/spry.dart';

import 'cluster.dart';

final app = Application.late();

void main() {
  scale(startServer);
}

void startServer() async {
  app.get('/', (request) {});
  app.post('/user', (request) {});
  app.get('/user/:name', (request) => request.params.get('name'));

  await app.run(address: "0.0.0.0", port: 3000, shared: true);
}
