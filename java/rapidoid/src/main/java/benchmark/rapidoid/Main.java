package benchmark.rapidoid;

import org.rapidoid.config.Conf;
import org.rapidoid.http.MediaType;
import org.rapidoid.setup.App;
import org.rapidoid.setup.On;

public class Main {


  public static void main(String[] args) {
    App.run(args);

    Conf.HTTP.set("maxPipeline", 128);
    Conf.HTTP.set("timeout", 0);
    Conf.HTTP.sub("mandatoryHeaders").set("connection", false);

    On.port(3000);

    setupSimpleHandlers();
  }

  private static void setupSimpleHandlers() {
    On.get("/").managed(false).contentType(MediaType.TEXT_PLAIN).serve("");
    On.get("/user/{id}").managed(false).contentType(MediaType.TEXT_PLAIN)
        .serve(id -> Integer.parseInt(id.param("id")));
    On.post("/user").managed(false).contentType(MediaType.TEXT_PLAIN).serve("");
  }

}
