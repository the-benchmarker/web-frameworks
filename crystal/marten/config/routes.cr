Marten.routes.draw do
  path "/", RootHandler, name: "root"
  path "/user", UserCreateHandler, name: "user_create"
  path "/user/<id:str>", UserDetailHandler, name: "user_detail"
end
