case class MinimalRoutes()(implicit cc: castor.Context, log: cask.Logger) extends cask.Routes{

  @cask.get("/")
  def hello() = {
    ""
  }

  @cask.post("/user")
  def postUser(request: cask.Request) = {
    ""
  }

  @cask.get("/user/:name") // variable path segment, e.g. HOST/user/lihaoyi
  def getUser(name: String) = {
    name
  }

  initialize()
}


object MinimalRoutesMain extends cask.Main {
  override val port = 3000
  override val host = "0.0.0.0"

  val allRoutes = Seq(MinimalRoutes())
}
