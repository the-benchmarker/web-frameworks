package web.helidon;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;

@Path("/user")
public class UserResource {

    @Path("/{id}")
    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String getMessageHandler(@PathParam("id") String id) {
        return id;
    }

    @POST
    @Produces(MediaType.TEXT_PLAIN)
    public String getDefaultMessageHandler() {
        return "";
    }


}
