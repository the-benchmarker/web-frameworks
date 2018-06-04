package benchmark.act;

import act.Act;
import act.handler.NonBlock;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.annotation.SessionFree;

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
