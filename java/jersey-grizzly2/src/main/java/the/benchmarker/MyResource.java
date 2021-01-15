package the.benchmarker;

import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

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
