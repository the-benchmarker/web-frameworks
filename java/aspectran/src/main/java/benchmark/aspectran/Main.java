package benchmark.aspectran;

import com.aspectran.daemon.DefaultDaemon;

public class Main {

    public static void main(String[] args) {
        try {
            DefaultDaemon.main(args);
        } catch (Exception e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

}
