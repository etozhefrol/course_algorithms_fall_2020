package task1

import vegas.{Layer, Line, Quant, Vegas}

import scala.util.Random

object SubTask2 {
  val MAX = 10000

  var practical: Seq[Map[String, Long]] = Seq()

  def main(args: Array[String]): Unit = {
    val approx: Array[Long] = Array.fill(2000)(0)

    for {
      run <- 1 to 5
      n <- 1 to 400
    } {
      val A: Array[Array[Int]] = generateMatrix(n)
      val B: Array[Array[Int]] = generateMatrix(n)

      val start: Long = System.nanoTime()

      matrixProduct(A, B)

      val end: Long = System.nanoTime()

      val res = end - start

      approx(n - 1) += res

      println(n)
    }

    plotGraph(approx)
  }

  def generateMatrix(n: Int): Array[Array[Int]] = Array.fill(n, n)(Random.nextInt(MAX))

  def matrixProduct(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    val n = a.length

    val res: Array[Array[Int]] = Array.ofDim(n, n) // square matrices

    for {
      i <- 0 until n
      j <- 0 until n
      k <- 0 until n
    } {
      res(i)(j) += a(i)(k) * b(k)(j)
    }

    res
  }

  def plotGraph(array: Array[Long]): Unit = {
    var approxSeq: Seq[Map[String, Long]] = Seq()

    for (n <- 1 to 2000) {
      if (n > 1 && array(n - 1) < array(n - 2) * 2) {
        approxSeq = approxSeq :+ Map("n" -> n.toLong, "time (nanos)" -> array(n - 1) / 5)
      }
    }

    println("20: " + array(19) / 5)
    println("100: " + array(99) / 5)
    println("400: " + array(399) / 5)

    Vegas.layered("algs", width = 1000.0, height = 1000.0)
      .withLayers(
        Layer()
          .withData(approxSeq)
          .mark(Line)
          .encodeX("n", Quant)
          .encodeY("time (nanos)", Quant)
      ).show
  }
}
