package benchmark.act;

import act.Act;
import act.controller.ExpressController;
import org.osgl.http.H;
import org.osgl.mvc.annotation.*;

@SuppressWarnings("unused")
@ExpressController
@ResponseContentType(H.MediaType.TXT)
public class AppEntry {

    @GetAction("/")
    public String index(String id) {
        return "";
    }

    @GetAction("/user/{id}")
    public String user(String id) {
        return id;
    }

    @ResponseStatus(200)
    @PostAction("/user")
    public String create() {
        return "";
    }

    public static void main(String[] args) throws Exception {
        Act.start();
    }

}
