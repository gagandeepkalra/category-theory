package examples.server.proxy

import java.net.ServerSocket

import examples.server.reader.InputReader

class SimpleServer(port: Int) extends Thread with InputReader {
  val socket = new ServerSocket(port)
  override def run(): Unit = {
    while (true) {
      println("Server accepting connections...")
      val connectionSocket = socket.accept()
      println(s"Server Accepted Connection: $connectionSocket")

      val in = connectionSocket.getInputStream
      val message = readRecur(in)

      println(s"Received: $message")
      val response =
        """
            |HTTP/1.1 200 OK
            |
            |
            |Yay you win
            |""".stripMargin
      connectionSocket.getOutputStream.write(response.getBytes("UTF-8"))
      connectionSocket.shutdownOutput()
    }
  }
}
