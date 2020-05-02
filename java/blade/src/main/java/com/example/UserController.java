package com.example;

import com.blade.mvc.annotation.*;
import com.blade.mvc.ui.RestResponse;

@Path("user")
public class UserController {

    @PostRoute("/")
    @JSON
    public RestResponse user() {
        return RestResponse.ok("");
    }

    @GetRoute("/:id")
    @JSON
    public RestResponse users(@PathParam Integer id) {
        return RestResponse.ok(id);
    }

}

