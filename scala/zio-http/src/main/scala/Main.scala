import zio._
import zio.http._

object HelloWorld extends ZIOAppDefault {

  val app = Http.collect[Request] {
    case Method.GET -> Root => Response.ok
    case Method.POST -> Root / "user" => Response.ok
    case Method.GET -> Root / "user" / id => { Response.text(id) }
  }

  override val run =
    Server.serve(app).provide(Server.defaultWithPort(3000))

}
