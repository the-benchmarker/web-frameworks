package the.benchmarker.http4s

import cats.effect.Effect
import io.circe.Json
import org.http4s.HttpService
import org.http4s.dsl.Http4sDsl

class HelloWorldService[F[_]: Effect] extends Http4sDsl[F] {

  val service: HttpService[F] = {
    HttpService[F] {
      case GET -> Root =>
        Ok("")
      case POST -> Root / "user" =>
        Ok("")
      case GET -> Root / "user" / name =>
        Ok(name)
    }
  }
}
