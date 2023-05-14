import happyx


serve("0.0.0.0", 3000):
  get "/":
    ""
  
  get "/user$id?:int":
    "method not allowed"
  
  post "/user":
    "method not allowed"
  
  notfound:
    "method not allowed"
