import zhttp.http._
import zhttp.service.Server
import zio._

object HelloWorld extends App {

  val app = Http.collect[Request] {
    case Method.GET -> Root => Response.text("")
    case Method.POST -> Root / "user" => Response.text("")
    case req @ Method.GET -> Root / "user" / _ => {
      Response.text(req.route._2.asString.split('/').last)
    }
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    Server.start(3000, app).exitCode

}