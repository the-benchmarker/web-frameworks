package org.apache.struts.benchmarker.example;

import static com.opensymphony.xwork2.Action.SUCCESS;

public class UserAction {

    private String id;

    public String index() {
        return SUCCESS  ;
    }

    public String view() {
        return "user-id";
    }

    public String create() {
        return SUCCESS;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }
}
