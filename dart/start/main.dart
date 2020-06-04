import 'package:start/start.dart';

void main() {
  start(host: '0.0.0.0', port: 3000).then((Server app) {

    app.get('/').listen((request) {
      request.response
        .send('');
    });

    app.post('/user').listen((request) {
      request.response
        .send('');
    });

    app.get('/user/:id').listen((request) {
      request.response
        .send('${request.param('id')}');
    });

  });
}

