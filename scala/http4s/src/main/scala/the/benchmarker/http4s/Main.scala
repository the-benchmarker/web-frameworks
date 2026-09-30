package the.benchmarker.http4s

import cats.effect.*
import com.comcast.ip4s.*
import org.http4s.HttpRoutes
import org.http4s.Response
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*

import scala.concurrent.ExecutionContext.global

object Main extends IOApp {

  val okEmpty: IO[Response[IO]] = Ok("")

  val helloWorldService = HttpRoutes
    .of[IO] {
      case GET -> Root =>
        okEmpty
      case POST -> Root / "user" =>
        okEmpty
      case GET -> Root / "user" / name =>
        Ok(name)
    }
    .orNotFound

  def run(args: List[String]): IO[ExitCode] =
    EmberServerBuilder
      .default[IO]
      .withHost(host"0.0.0.0")
      .withPort(port"3000")
      .withHttpApp(helloWorldService)
      .build
      .useForever
      .as(ExitCode.Success)
}
