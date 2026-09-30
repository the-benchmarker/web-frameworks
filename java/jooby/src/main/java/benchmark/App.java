package benchmark;

import static io.jooby.ExecutionMode.EVENT_LOOP;

import io.jooby.Jooby;
import io.jooby.ServerOptions;
import io.jooby.StatusCode;
import io.jooby.netty.NettyServer;

public class App extends Jooby {
  {
    get("/", ctx -> ctx.send(StatusCode.OK));
    get("/user/{id}", ctx -> ctx.send(ctx.path("id").value()));
    post("/user", ctx -> ctx.send(StatusCode.OK));
  }

  public static void main(String[] args) {
    var options = new ServerOptions().setPort(3000).setDefaultHeaders(false);
    runApp(args, new NettyServer(options), EVENT_LOOP, App::new);
  }
}
