package org.acme;

import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/")
public class BenchmarkResource {

    @GET
    @Path("/")
    @Produces(MediaType.TEXT_PLAIN)
    public String root() {
        return "";
    }

    @GET
    @Path("/user/{id}")
    @Produces(MediaType.TEXT_PLAIN)
    public String userId(@PathParam("id") String id) {
        return id;
    }

    @POST
    @Path("/user")
    @Produces(MediaType.TEXT_PLAIN)
    public String user() {
        return "";
    }
}