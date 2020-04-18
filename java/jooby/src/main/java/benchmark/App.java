package benchmark;

import io.jooby.Jooby;
import io.jooby.ServerOptions;
import io.jooby.StatusCode;

import static io.jooby.ExecutionMode.EVENT_LOOP;

public class App extends Jooby {
  {
    ServerOptions options = new ServerOptions();
    options.setPort(3000);
    // Turn off Date and Server Response headers.
    options.setDefaultHeaders(false);
    setServerOptions(options);

    get("/", ctx -> ctx.send(StatusCode.OK));
    get("/user/{id}", ctx -> ctx.send(ctx.path("id").value()));
    get("/user", ctx -> ctx.send(StatusCode.OK));
  }

  public static void main(String[] args) {
    runApp(args, EVENT_LOOP, App::new);
  }
}
