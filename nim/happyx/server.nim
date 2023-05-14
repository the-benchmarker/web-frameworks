import happyx


serve("0.0.0.0", 3000):
  get "/":
    ""
  
  get "/user$id:int":
    "{id}"
  
  post "/user":
    ""
  
  notfound:
    "method not allowed"
