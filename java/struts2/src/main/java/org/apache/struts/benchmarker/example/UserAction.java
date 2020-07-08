package org.apache.struts.benchmarker.example;

import org.apache.struts2.result.PlainResult;

public class UserAction {

    private String id;

    public PlainResult execute() {
        return response -> response.write("");
    }

    public PlainResult index() {
        return response -> response.write("");
    }

    public PlainResult view() {
        return response -> response.write(id);
    }

    public PlainResult create() {
        return response -> response.write("");
    }

    public void setId(String id) {
        this.id = id;
    }

}
