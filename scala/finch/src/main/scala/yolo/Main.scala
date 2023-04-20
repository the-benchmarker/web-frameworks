package yolo

import cats.effect.{IO, IOApp, ExitCode}
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.Await

import io.finch._

object Main extends IOApp with Endpoint.Module[IO] {

  val okEmpty: Output[String] = Ok("")

  val root = get(zero) {
    okEmpty
  }

  val postUser = post("user") {
    okEmpty
  }

  val getUserName = get("user" :: path[String]) { name: String => Ok(name) }

  override def run(args: List[String]): IO[ExitCode] =
    Bootstrap[IO]
      .serve[Text.Plain](postUser :+: getUserName :+: root)
      .listen("0.0.0.0:3000")
      .useForever

}
