package examples.server.http

import java.net.InetSocketAddress
import java.util.concurrent.{ExecutorService, Executors}

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import examples.server.reader.InputReader
import org.apache.http.HttpHost
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.{HttpClients, LaxRedirectStrategy}
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager
import org.apache.http.protocol.BasicHttpContext

class SimpleHttpServer(host: String,
                       port: Int,
                       pool: Int = 10,
                       response: String = "Hello")
    extends Thread {

  private val threadPoolExecutor: ExecutorService =
    Executors.newFixedThreadPool(pool)
  private val server =
    HttpServer.create(new InetSocketAddress(host, port), 0);

  server.createContext("/", new DefaultHandler(response));
  server.setExecutor(threadPoolExecutor);

  override def run(): Unit = server.start()

  def close(): Unit = {
    server.stop(1)
    threadPoolExecutor.shutdown()
  }

}

class DefaultHandler(message: String) extends HttpHandler {
  override def handle(httpExchange: HttpExchange): Unit = {
    httpExchange.sendResponseHeaders(200, message.getBytes.length)
    httpExchange.getResponseBody.write(message.getBytes())
    httpExchange.getResponseBody.flush()
    httpExchange.getResponseBody.close()
  }
}

object SimpleHttpServer extends InputReader {
  def main(args: Array[String]): Unit = {

    val host = "localhost"
    val port = 8001

    val server = new SimpleHttpServer(host, port)
    server.start()

    val proxyHost = new HttpHost(host, port, "http")

    val httpClient = HttpClients.custom
      .useSystemProperties()
      .setConnectionManager(new PoolingHttpClientConnectionManager)
      .setRedirectStrategy(LaxRedirectStrategy.INSTANCE)
      .setProxy(proxyHost)
      .build()

    val httpContext = new BasicHttpContext
    val httpGet = new HttpGet("http://www.google.com")
    val response = httpClient.execute(httpGet, httpContext)

    println(readRecur(response.getEntity.getContent))

    server.close()

  }
}
