import com.twitter.finatra._
import com.twitter.finagle.Http.Server
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.stack.nilStack
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finatra.http.HttpServer
import com.twitter.finatra.http.routing.HttpRouter

object HelloWorldServerMain extends WebServer

class WebServer extends HttpServer {

  override val defaultHttpPort: String = ":3000"

  override def configureHttpServer(server: Server): Server = {
    server
      .withCompressionLevel(0)
      .withStatsReceiver(NullStatsReceiver)
      .withStack(nilStack[Request, Response])
  }

  override def configureHttp(router: HttpRouter) = {
    router.add(new AController)
  }
}
