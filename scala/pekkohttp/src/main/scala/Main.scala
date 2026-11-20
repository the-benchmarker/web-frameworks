import org.apache.pekko
import pekko.actor.ActorSystem
import pekko.http.scaladsl.Http
import pekko.http.scaladsl.server.Directives._

object Main {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("PekkoHttp")

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

    Http().newServerAt("0.0.0.0", 3000).bind(route)
  }
}
