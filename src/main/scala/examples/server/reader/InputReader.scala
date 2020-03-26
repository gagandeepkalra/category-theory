package examples.server.reader

import java.io.InputStream

import scala.annotation.tailrec

trait InputReader {
  @tailrec
  final def readRecur(in: InputStream,
                      sb: StringBuilder = new StringBuilder): String = {
    val input = in.read()
    if (input != -1) {
      readRecur(in, sb.append(input.toChar))
    } else sb.toString()
  }
}