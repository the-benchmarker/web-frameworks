import mike

"/" -> get:
  result = ""

"/user/:id" -> get:
  result = ctx.pathParams["id"] 

"/user" -> post:
  result = ""

run(3000)
