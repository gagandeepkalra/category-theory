package examples.server.proxy

import java.net.Socket

import examples.server.proxy.tcp.{ProxyServer, SimpleServer}
import examples.server.reader.InputReader

object proxy extends InputReader {
  def main(args: Array[String]): Unit = {
    val server = new SimpleServer(2222)
    println("starting server")
    server.start()

    val proxyServer = new ProxyServer("0.0.0.0", 2222, 3333)
    println("starting proxy server")
    proxyServer.start()

    // Begin testing
    val proxyServerSocket = new Socket("0.0.0.0", 2222)
    val message: String = "Hello!"
    println(s"Client sending message: $message")
    proxyServerSocket.getOutputStream.write(message.getBytes("UTF-8"))
    proxyServerSocket.shutdownOutput()

    val response = readRecur(proxyServerSocket.getInputStream)
    println(s"Client received: $response")
  }

}
