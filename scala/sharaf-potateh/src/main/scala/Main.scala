import ba.sake.potateh.ServerConfig
import ba.sake.sharaf.*
import ba.sake.sharaf.potateh.PotatehSharafServer
import java.net.InetSocketAddress
import java.util.concurrent.{CountDownLatch, Executors}

object Main {
  def main(args: Array[String]): Unit = {
    val routes = Routes {
      case GET -> Path()           => Response.default
      case GET -> Path("user", id) => Response.withBody(id)
      case POST -> Path("user")    => Response.default
    }
    val config = ServerConfig.Default
      .withBindAddress(new InetSocketAddress("0.0.0.0", 3000))
      .withMaxConnections(1024)
    val virtualThreads = Executors.newVirtualThreadPerTaskExecutor()
    val handler = SharafHandler.exceptions(SharafHandler.routes(routes), ExceptionMapper.default)
    val server = PotatehSharafServer.start(config, handler, virtualThreads)
    Runtime.getRuntime.addShutdownHook(new Thread(() => { server.stop(); virtualThreads.shutdownNow(); () }, "sharaf-potateh-shutdown"))
    println("Sharaf with Potateh JVM listening on port 3000")
    new CountDownLatch(1).await()
  }
}
