package examples.server.proxy.tcp

import java.net.{ServerSocket, Socket}

import examples.server.reader.InputReader

class ProxyServer(address: String, port: Int, proxyPort: Int)
    extends Thread
    with InputReader {
  val proxySocket = new ServerSocket(proxyPort)

  override def run(): Unit = {
    while (true) {
      println("Proxy accepting connections...")
      val connectionSocket = proxySocket.accept()
      val connectionInputStream = connectionSocket.getInputStream
      println(s"Proxy Accepted Connection: $connectionSocket")

      val serverSocket = new Socket(address, port)
      val serverInputStream = serverSocket.getInputStream

      val message: String = readRecur(connectionInputStream)
      println(s"Proxy server received: $message")
      serverSocket.getOutputStream.write(message.getBytes("UTF-8"))
      serverSocket.shutdownOutput()

      val response = readRecur(serverInputStream)
      println(s"Proxy server received $response from server")
      connectionSocket.getOutputStream.write(response.getBytes("UTF-8"))
      connectionSocket.shutdownOutput()
    }
  }
}
