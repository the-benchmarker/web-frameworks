import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._

object Main {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("AkkaHttp")

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

    Http().newServerAt(interface = "0.0.0.0", port = 3000).bind(route)
  }
}
