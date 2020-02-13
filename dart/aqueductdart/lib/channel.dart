import 'aqueductdart.dart';

class AqueductDartChannel extends ApplicationChannel {
  @override
  Controller get entryPoint {
    final router = Router();

    router.route("/").linkFunction((request) => Response.ok(""));

    router.route("/users/:id").linkFunction(
        (Request request) => Response.ok(request.path.variables['id']));

    router.route("/users").linkFunction((request) => Response.ok(""));

    return router;
  }
}
