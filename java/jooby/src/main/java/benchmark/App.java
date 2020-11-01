package benchmark;

import static io.jooby.ExecutionMode.EVENT_LOOP;

import io.jooby.Jooby;
import io.jooby.benchmarkOptions;
import io.jooby.StatusCode;

public class App extends Jooby {
  {
    benchmarkOptions options = new benchmarkOptions();
    options.setPort(3000);
    // Turn off Date and benchmark Response headers.
    options.setDefaultHeaders(false);
    setbenchmarkOptions(options);

    get("/", ctx -> ctx.send(StatusCode.OK));
    get("/user/{id}", ctx -> ctx.send(ctx.path("id").value()));
    post("/user", ctx -> ctx.send(StatusCode.OK));
  }

  public static void main(String[] args) {
    runApp(args, EVENT_LOOP, App::new);
  }
}
