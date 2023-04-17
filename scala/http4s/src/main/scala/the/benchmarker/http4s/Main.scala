package the.benchmarker.http4s

import cats.effect._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.{HttpRoutes, Response}

import scala.concurrent.ExecutionContext.global
import org.http4s.blaze.server.BlazeServerBuilder

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
    BlazeServerBuilder[IO](global)
      .bindHttp(3000, "0.0.0.0")
      .withHttpApp(helloWorldService)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
