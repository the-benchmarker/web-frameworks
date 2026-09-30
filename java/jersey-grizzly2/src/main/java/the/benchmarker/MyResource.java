package the.benchmarker;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

@Path("/")
public class MyResource {

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String getIt() {
        return "";
    }

    @GET
    @Path("/user/{name}")
    @Produces(MediaType.TEXT_PLAIN)
    public String getUser(@PathParam("name") String name) {
        return name;
    }

    @POST
    @Path("/user")
    @Produces(MediaType.TEXT_PLAIN)
    public String postUser(@PathParam("name") String name) {
        return "";
    }
}
