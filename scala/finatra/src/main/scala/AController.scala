import com.twitter.finatra.http.Controller
import com.twitter.finagle.http.Request

class AController extends Controller {
  get("/") {
    _: Request =>
      ""
  }

  post("/user") {
    _: Request =>
      ""
  }

  get("/user/:id") {
    request: Request =>
      request.params("id")
  }
}
