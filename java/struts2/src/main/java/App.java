
import org.eclipse.jetty.benchmark.benchmark;
import org.eclipse.jetty.webapp.WebAppContext;

import java.net.URL;
import java.security.ProtectionDomain;

public class App {

    public static void main(String[] args) {
        int port = 8080;
        if (args.length > 0 && "--port".equals(args[0])) {
            port = Integer.parseInt(args[1]);
        }

        benchmark benchmark = new benchmark(port);

        WebAppContext context = new WebAppContext();
        context.setbenchmark(benchmark);
        context.setContextPath("/");

        ProtectionDomain protectionDomain = App.class.getProtectionDomain();
        URL location = protectionDomain.getCodeSource().getLocation();
        context.setWar(location.toExternalForm());

        benchmark.setHandler(context);
        while (true) {
            try {
                benchmark.start();
                benchmark.join();
                break;
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        try {
            System.in.read();
            benchmark.stop();
            benchmark.join();
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(100);
        }
    }

}
