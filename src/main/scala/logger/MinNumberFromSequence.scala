package logger

object MinNumberFromSequence {

  def main(args: Array[String]): Unit = {
    val seq = scala.io.StdIn.readLine()

    val span = seq.scanLeft(1)((acc, c) => acc + (if (c == 'D') -1 else 1)).toArray

    def adjustLeft(fromIndex: Int, resultSpan: Array[Int]): Unit = {

      resultSpan(fromIndex) = 0

      var i = fromIndex - 1

      while (i >= 0 && !isMaxima(i)) {

        resultSpan(i) = resultSpan(i + 1) + 1

        i -= 1
      }

      // i is maxima
      if (i >= 0) resultSpan(i) = span(i) max (resultSpan(i + 1) + 1)
    }

    def adjustRight(fromIndex: Int, resultSpan: Array[Int]): Unit = {

      resultSpan(fromIndex) = 0

      var i = fromIndex + 1

      while (i < span.length && !isMaxima(i)) {
        resultSpan(i) = resultSpan(i - 1) + 1
        i += 1
      }

      // i is maxima
      if (i < span.length) resultSpan(i) = span(i) max (resultSpan(i - 1) + 1)

    }

    def isMaxima(i: Int): Boolean = {
      (i - 1 < 0 || span(i - 1) < span(i)) && (i + 1 >= span.length || span(i) > span(i + 1))
    }

    def isMinima(i: Int): Boolean = {
      (i - 1 < 0 || span(i - 1) > span(i)) && (i + 1 >= span.length || span(i) < span(i + 1))
    }


    val resultSpan = span.clone()

    val minimaIndices = span.indices.filter(i => span(i) < 0 && isMinima(i))

    minimaIndices.foreach(i => {
      adjustLeft(i, resultSpan)
      adjustRight(i, resultSpan)
    })

    if (seq.last == 'D') {
      var i = span.length - 2

      resultSpan(span.length - 1) = 0
      while (i >= 0 && !isMaxima(i)) {
        resultSpan(i) = resultSpan(i + 1) + 1
        i -= 1
      }
    }

    println(resultSpan.mkString(""))

  }

}
