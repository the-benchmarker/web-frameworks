package benchmark.dropwizard;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

@Path("/")
@Produces(MediaType.TEXT_PLAIN)
public class UserResource {

  @GET
  public String index() {
    return "";
  }

  @GET
  @Path("/user/{id}")
  public String getUser(@PathParam("id") String id) {
    return id;
  }

  @POST
  @Path("/user")
  public String createUser() {
    return "";
  }
}
