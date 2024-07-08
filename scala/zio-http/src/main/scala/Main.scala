import zio._
import zio.http._
import zio.http.codec.PathCodec.string

object HelloWorld extends ZIOAppDefault {

  val x = Method.GET / "user" / string("id") -> handler {
    (id: String, _: Request) =>
      Response.text(s"$id")
  }

  val y = Method.GET / "" -> handler(Response.ok)

  val z = Method.POST / "user" -> handler(Response.ok)

  val app = Routes(x, y, z)

  override val run =
    Server.serve(app).provide(Server.defaultWithPort(3000))

}
