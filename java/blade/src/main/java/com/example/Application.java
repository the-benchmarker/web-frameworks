import com.blade.Blade;
import com.blade.mvc.http.HttpMethod;
import com.blade.mvc.http.Response;

public class BenchmarkApplication {

    public static void main(String[] args) {
        Blade.of()
            // GET /
            .get("/", ctx -> {
                ctx.text(""); // returns empty response
            })

            // GET /user/:id
            .get("/user/:id", ctx -> {
                Integer id = ctx.pathInt("id");
                ctx.text(String.valueOf(id));
            })

            // POST /user
            .post("/user", ctx -> {
                ctx.text(""); // empty response
            })

            .start(BenchmarkApplication.class, args);
    }
}
