import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer


object Main {

  def main(args: Array[String]) {
    implicit val system = ActorSystem("AkkaHttp")
    implicit val materializer = ActorMaterializer()

    val route =
      pathSingleSlash {
        complete("")
      } ~
        path("user") {
          post {
            complete("")
          }
        } ~
        path("user" / Remaining) { id =>
          complete(id)
        }


    Http().bindAndHandle(route, "localhost", 3000)
  }

}
