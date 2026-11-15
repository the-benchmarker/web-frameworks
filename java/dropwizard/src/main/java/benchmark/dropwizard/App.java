package benchmark.dropwizard;

import io.dropwizard.configuration.ResourceConfigurationSourceProvider;
import io.dropwizard.core.Application;
import io.dropwizard.core.Configuration;
import io.dropwizard.core.setup.Bootstrap;
import io.dropwizard.core.setup.Environment;

public class App extends Application<Configuration> {
  public static void main(String[] args) throws Exception {
    new App().run("server", "config.yml");
  }

  @Override
  public void initialize(Bootstrap<Configuration> bootstrap) {
    bootstrap.setConfigurationSourceProvider(new ResourceConfigurationSourceProvider());
  }

  @Override
  public void run(Configuration configuration, Environment environment) {
    environment.jersey().register(new UserResource());
  }
}
