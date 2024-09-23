package yolo;


import io.smallrye.mutiny.Uni;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import org.jboss.logging.Logger;

@Path("/")
public class BenchmarkResource {
    private static final Logger log = Logger.getLogger(BenchmarkResource.class);
    @GET
    @Path("/")
    @Produces(MediaType.TEXT_PLAIN)
    public Uni<String> root() {
        return Uni.createFrom().item("");
    }

    @GET
    @Path("/user/{id}")
    @Produces(MediaType.TEXT_PLAIN)
    public Uni<String> userId(String id) {
        return Uni.createFrom().item(id);
    }

    @POST
    @Path("/user")
    @Produces(MediaType.TEXT_PLAIN)
    public Uni<String> user() {
        return Uni.createFrom().item("");
    }
}