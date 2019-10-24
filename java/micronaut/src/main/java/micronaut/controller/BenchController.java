package example.micronaut;

import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.Post;
import io.micronaut.http.annotation.Produces;

@Controller("/") 
public class BenchController {
    @Get("/") 
    @Produces(MediaType.TEXT_PLAIN) 
    public String index() {
        return ""; 
    }

    @Get("/user/{id}") 
    @Produces(MediaType.TEXT_PLAIN) 
    public String index(String id) {
        return id; 
    }

    @Post("/user")
    public String post(){
        return "";
    }
}
