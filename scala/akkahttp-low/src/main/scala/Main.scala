import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer

object Main {

  def main(args: Array[String]) {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val requestHandler: HttpRequest => HttpResponse = {
      case HttpRequest(GET, Uri.Path("/"), _, _, _) =>
        HttpResponse(entity = "")
      case HttpRequest(POST, Uri.Path("/user"), _, _, _) =>
        HttpResponse(entity = "")
      case HttpRequest(GET, path, _, _, _) if path.path.toString().startsWith("/user/")  =>
        HttpResponse(entity = path.toString().split("/").last)
      case r: HttpRequest =>
        r.discardEntityBytes() // important to drain incoming HTTP Entity stream
        HttpResponse(404, entity = "Unknown resource!")
    }

    Http().bindAndHandleSync(requestHandler, "localhost", 3000)

    println(s"Server online at http://localhost:3000")
  }

}
