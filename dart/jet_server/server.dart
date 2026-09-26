import 'dart:io' show Platform;
import 'dart:isolate' show Isolate;
import 'dart:typed_data' show Uint8List;

import 'package:jet_server/jet_server.dart' show HttpRequest, JetServer;

final _okKeepAlive = Uint8List.fromList(
  'HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'
      .codeUnits,
);

final _okClose = Uint8List.fromList(
  'HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.codeUnits,
);

final _notFoundKeepAlive = Uint8List.fromList(
  'HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'
      .codeUnits,
);

final _notFoundClose = Uint8List.fromList(
  'HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'
      .codeUnits,
);

// 'HTTP/1.1 200 OK\r\nContent-Length: '
final _userPrefix = Uint8List.fromList(
  'HTTP/1.1 200 OK\r\nContent-Length: '.codeUnits,
);

// '\r\nConnection: keep-alive\r\n\r\n'
final _userMidKeepAlive = Uint8List.fromList(
  '\r\nConnection: keep-alive\r\n\r\n'.codeUnits,
);

// '\r\nConnection: close\r\n\r\n'
final _userMidClose = Uint8List.fromList(
  '\r\nConnection: close\r\n\r\n'.codeUnits,
);

// ASCII length digits.
final _lengthDigits = List<Uint8List>.generate(128, (i) {
  if (i < 10) return Uint8List.fromList([0x30 + i]);
  return Uint8List.fromList(i.toString().codeUnits);
});

@pragma('vm:always-consider-inlining')
Uint8List _digits(int n) =>
    n < 128 ? _lengthDigits[n] : Uint8List.fromList(n.toString().codeUnits);

Uint8List handleRequest(HttpRequest req) {
  final buf = req.buffer;
  final methodLen = req.method.len;
  final methodStart = req.method.start;
  final pathLen = req.path.len;
  final pathStart = req.path.start;

  // GET
  if (methodLen == 3 &&
      buf[methodStart] == 0x47 &&
      buf[methodStart + 1] == 0x45 &&
      buf[methodStart + 2] == 0x54) {
    if (pathLen == 1 && buf[pathStart] == 0x2F) {
      return req.keepAlive ? _okKeepAlive : _okClose;
    }

    // /user/:id (path starts with /user/)
    if (pathLen > 6 &&
        buf[pathStart] == 0x2F &&
        buf[pathStart + 1] == 0x75 &&
        buf[pathStart + 2] == 0x73 &&
        buf[pathStart + 3] == 0x65 &&
        buf[pathStart + 4] == 0x72 &&
        buf[pathStart + 5] == 0x2F) {
      final idLen = pathLen - 6;
      final mid = req.keepAlive ? _userMidKeepAlive : _userMidClose;
      final digits = _digits(idLen);
      final res = Uint8List(
        _userPrefix.length + digits.length + mid.length + idLen,
      );
      var o = 0;
      res.setRange(o, o + _userPrefix.length, _userPrefix);
      o += _userPrefix.length;
      res.setRange(o, o + digits.length, digits);
      o += digits.length;
      res.setRange(o, o + mid.length, mid);
      o += mid.length;
      final idStart = pathStart + 6;
      for (var i = 0; i < idLen; i++) {
        res[o + i] = buf[idStart + i];
      }
      return res;
    }

    return req.keepAlive ? _notFoundKeepAlive : _notFoundClose;
  }

  // POST /user
  if (methodLen == 4 &&
      buf[methodStart] == 0x50 &&
      buf[methodStart + 1] == 0x4F &&
      buf[methodStart + 2] == 0x53 &&
      buf[methodStart + 3] == 0x54) {
    if (pathLen == 5 &&
        buf[pathStart] == 0x2F &&
        buf[pathStart + 1] == 0x75 &&
        buf[pathStart + 2] == 0x73 &&
        buf[pathStart + 3] == 0x65 &&
        buf[pathStart + 4] == 0x72) {
      return req.keepAlive ? _okKeepAlive : _okClose;
    }
  }

  return req.keepAlive ? _notFoundKeepAlive : _notFoundClose;
}

void main() {
  for (var i = 0; i < Platform.numberOfProcessors - 1; i++) {
    Isolate.spawn((_) => startServer(), null);
  }
  startServer();
}

void startServer() {
  JetServer(handler: handleRequest, port: 3000, reusePort: true).serve();
}
