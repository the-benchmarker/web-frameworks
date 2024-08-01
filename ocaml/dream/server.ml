let index _request = Dream.html ""
let user_info request = Dream.param request "id" |> Dream.html
let user _request = Dream.html ""

let () =
  let open Dream in
  run ~interface:"0.0.0.0" ~port:3000
  @@ router [ get "/" index; get "/user/:id" user_info; post "/user" user ]
