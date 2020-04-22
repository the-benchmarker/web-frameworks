package the.benchmarker.http4s

import cats.effect._
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.syntax._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._

object Main extends IOApp {
  val helloWorldService = HttpRoutes.of[IO] {
    case GET -> Root =>
      Ok("")
    case POST -> Root / "user" =>
      Ok("")
    case GET -> Root / "user" / name =>
      Ok(name)
  }.orNotFound

  def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(3000, "0.0.0.0")
      .withHttpApp(helloWorldService)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
