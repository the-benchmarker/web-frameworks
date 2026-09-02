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
      final conn = req.keepAlive ? 'keep-alive' : 'close';
      final headerStr =
          'HTTP/1.1 200 OK\r\nContent-Length: $idLen\r\nConnection: $conn\r\n\r\n';
      final headerBytes = headerStr.codeUnits;
      final totalLen = headerBytes.length + idLen;
      final res = Uint8List(totalLen)..setAll(0, headerBytes);
      final idStart = pathStart + 6;
      for (var i = 0; i < idLen; i++) {
        res[headerBytes.length + i] = buf[idStart + i];
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
