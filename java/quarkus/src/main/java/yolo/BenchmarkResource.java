package yolo;


import io.smallrye.common.annotation.NonBlocking;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

@Path("/")
public class BenchmarkResource {
    @GET
    @Path("/")
    @Produces(MediaType.TEXT_PLAIN)
    @NonBlocking
    public String root() {
        return "";
    }

    @GET
    @Path("/user/{id}")
    @Produces(MediaType.TEXT_PLAIN)
    @NonBlocking
    public String userId(String id) {
        return id;
    }

    @POST
    @Path("/user")
    @Produces(MediaType.TEXT_PLAIN)
    @NonBlocking
    public String user() {
        return "";
    }
}