package benchmark.act;

import act.Act;
import act.handler.NonBlock;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.annotation.SessionFree;

/**
 * A simple hello world service app entry
 *
 * Run this app, try to update some of the code, then
 * press F5 in the browser to watch the immediate change
 * in the browser!
 */
@SuppressWarnings("unused")
public class AppEntry {

    @GetAction("/user/{id}")
    @SessionFree
    @NonBlock
    public String user(String id) {
        return id;
    }

    public static void main(String[] args) throws Exception {
        Act.start();
    }

}
