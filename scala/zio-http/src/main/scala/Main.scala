import zio._
import zio.http._

object HelloWorld extends ZIOAppDefault {
  val one =
    Method.GET / "" -> handler(Response.ok)

  val two =
    Method.POST / "" -> handler(Response.ok)

  val three = Method.GET / "user" / string("id") -> handler { (id: String, req: Request) =>
    Response.text(id)
  }

  val app = Routes(one, two, three).toHttpApp

  override val run = Server.serve(app).provide(Server.default)
}
