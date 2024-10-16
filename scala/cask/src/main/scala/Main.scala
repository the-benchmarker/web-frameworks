case class MinimalRoutes()(implicit cc: castor.Context, log: cask.Logger) extends cask.Routes{


  @cask.get("/")
  def hello() = {
    "Hello World!"
  }

  @cask.post("/do-thing")
  def doThing(request: cask.Request) = {
    request.text().reverse
  }

  initialize()
}


object MinimalRoutesMain extends cask.Main{
  val port = 3000

  val allRoutes = Seq(MinimalRoutes())
}