package yolo

import cats.effect.IO
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.Await

import io.finch._

object Main extends App with Endpoint.Module[IO] {

  val root = get(zero) {
    Ok("")
  }

  val postUser = post("user") {
    Ok("")
  }

  val getUserName = get("user" :: path[String]) { name: String => Ok(name) }

  val api: Service[Request, Response] =
    Bootstrap
      .serve[Text.Plain](root)
      .serve[Text.Plain](postUser)
      .serve[Text.Plain](getUserName)
      .toService

  Await.ready(
    Http.serve("localhost:9000", api)
  )
}
