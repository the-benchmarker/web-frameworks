package yolo

import cats.effect.IO
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.Await

import io.finch._

object Main extends App with Endpoint.Module[IO] {

  val okEmpty: Output[String] = Ok("")

  val root = get(zero) {
    okEmpty
  }

  val postUser = post("user") {
    okEmpty
  }

  val getUserName = get("user" :: path[String]) { name: String => Ok(name) }

  val api: Service[Request, Response] =
    Bootstrap
      .serve[Text.Plain](postUser :+: getUserName :+: root)
      .toService

  Await.ready(
    Http.serve("0.0.0.0:3000", api)
  )
}
