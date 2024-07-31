let index _request = Dream.html ""

let user_info request =
  match Dream.param request "id" |> int_of_string_opt with
  | None -> Dream.empty `Not_Found
  | Some id -> id |> string_of_int |> Dream.html

let user _request = Dream.html ""

let () =
  let open Dream in
  run ~port:3000
  @@ router [ get "/" index; get "/user/:id" user_info; post "/user" user ]
