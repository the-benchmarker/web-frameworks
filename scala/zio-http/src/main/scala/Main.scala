
import zio._
import zio.http._

object HelloWorld extends ZIOAppDefault {

  val app = Http.collect[Request] {
          case Method.GET -> !! => Response.ok
          case Method.POST -> !! / "user" => Response.ok
          case Method.GET -> !! / "user" / id => { Response.text(id) }
  }

  override val run =
    
  Server.serve(app).provide(Server.defaultWithPort(3000))

}
