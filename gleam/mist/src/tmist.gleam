import gleam/bytes_builder
import gleam/erlang/process
import gleam/http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/otp/actor
import gleam/result
import gleam/string
import mist.{type Connection, type ResponseData}

pub fn main() {
  let selector = process.new_selector()
  let state = Nil

  let not_found =
    response.new(404)
    |> response.set_body(mist.Bytes(bytes_builder.new()))

  let assert Ok(_) =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      case request.path_segments(req) {
        [] -> index(req)
        ["user", name] -> handle_user(req, name)
        ["user"] -> handle_user(req, "")
        _ -> not_found
      }
    }
    |> mist.new
    |> mist.bind("0.0.0.0")
    |> mist.port(3000)
    |> mist.start_http

  process.sleep_forever()
}

pub type MyMessage {
  Broadcast(String)
}

fn index(request: Request(Connection)) -> Response(ResponseData) {
  let content_type =
    request
    |> request.get_header("content-type")
    |> result.unwrap("text/plain")

  response.new(200)
  |> response.set_body(mist.Bytes(bytes_builder.new()))
}

fn handle_user(
  request: Request(Connection),
  name: String,
) -> Response(ResponseData) {
  let content_type =
    request
    |> request.get_header("content-type")
    |> result.unwrap("text/plain")

  case request.method {
    http.Post ->
      response.new(200)
      |> response.set_body(mist.Bytes(bytes_builder.new()))
    http.Get -> {
      response.new(200)
      |> response.set_body(mist.Bytes(bytes_builder.from_string(name)))
    }
    _other ->
      response.new(404)
      |> response.set_body(mist.Bytes(bytes_builder.new()))
  }
}
