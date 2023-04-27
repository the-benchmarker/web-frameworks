package yolo;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

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
    public String userId(String id) {
        return id;
    }

    @POST
    @Path("/user")
    @Produces(MediaType.TEXT_PLAIN)
    public String user() {
        return "";
    }
}