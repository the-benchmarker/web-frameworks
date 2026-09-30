import gleam/bytes_tree
import gleam/erlang/process
import gleam/http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/result
import mist.{type Connection, type ResponseData}

pub fn main() {
  let not_found =
    response.new(404)
    |> response.set_body(mist.Bytes(bytes_tree.new()))

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
    |> mist.start

  process.sleep_forever()
}

fn index(request: Request(Connection)) -> Response(ResponseData) {
  let content_type =
    request
    |> request.get_header("content-type")
    |> result.unwrap("text/plain")

  response.new(200)
  |> response.set_body(mist.Bytes(bytes_tree.new()))
  |> response.set_header("content-type", content_type)
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
      |> response.set_body(mist.Bytes(bytes_tree.new()))
      |> response.set_header("content-type", content_type)
    http.Get -> {
      response.new(200)
      |> response.set_body(mist.Bytes(bytes_tree.from_string(name)))
      |> response.set_header("content-type", content_type)
    }
    _other ->
      response.new(404)
      |> response.set_body(mist.Bytes(bytes_tree.new()))
      |> response.set_header("content-type", content_type)
  }
}
