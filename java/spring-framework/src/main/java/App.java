import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletHolder;
import org.eclipse.jetty.webapp.WebAppContext;
import org.springframework.web.context.ContextLoaderListener;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.AnnotationConfigWebApplicationContext;
import org.springframework.web.servlet.DispatcherServlet;

import java.net.URL;
import java.security.ProtectionDomain;

public class App {

    public static void main(String[] args) {
        int port = 8080;
        if (args.length > 0 && "--port".equals(args[0])) {
            port = Integer.parseInt(args[1]);
        }

        Server server = new Server(port);

        WebAppContext context = new WebAppContext();
        context.setServer(server);
        context.setContextPath("/");

        WebApplicationContext webAppContext = getWebApplicationContext();
        DispatcherServlet dispatcherServlet = new DispatcherServlet(webAppContext);
        ServletHolder springServletHolder = new ServletHolder("mvc-dispatcher", dispatcherServlet);
        context.addServlet(springServletHolder, "/*");
        context.addEventListener(new ContextLoaderListener(webAppContext));

        ProtectionDomain protectionDomain = App.class.getProtectionDomain();
        URL location = protectionDomain.getCodeSource().getLocation();
        context.setWar(location.toExternalForm());

        server.setHandler(context);
        while (true) {
            try {
                server.start();
                server.join();
                break;
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        try {
            System.in.read();
            server.stop();
            server.join();
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(100);
        }
    }

    private static WebApplicationContext getWebApplicationContext() {
        AnnotationConfigWebApplicationContext context = new AnnotationConfigWebApplicationContext();
        context.setConfigLocation("benchmark.springframework");
        return context;
    }

}
