package benchmark.act;

import act.Act;
import act.handler.NonBlock;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.annotation.PostAction;
import org.osgl.mvc.annotation.ResponseStatus;
import org.osgl.mvc.annotation.SessionFree;

@SuppressWarnings("unused")
public class AppEntry {

    @GetAction("/")
    @SessionFree
    @NonBlock
    public String index(String id) {
        return "";
    }

    @GetAction("/user/{id}")
    @SessionFree
    @NonBlock
    public String user(String id) {
        return id;
    }

    @ResponseStatus(200)
    @PostAction("/user")
    @SessionFree
    @NonBlock
    public String create() {
        return "";
    }

    public static void main(String[] args) throws Exception {
        Act.start();
    }

}
