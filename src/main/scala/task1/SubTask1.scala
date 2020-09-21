package task1

import vegas.{Layer, Line, Point, Quant, Vegas}

import scala.util.Random

object SubTask1 {
  val MAX = 10000
  val TASK = 8

  var practical: Seq[Map[String, Long]] = Seq()

  def main(args: Array[String]): Unit = {
    val approx: Array[Long] = Array.fill(2000)(0)

    for {
      run <- 1 to 5
      n <- 1 to 2000
    } {
      val array: Array[Double] = generateVector(n)
      val array2: Array[Double] = array.clone()
      val array3: Array[Double] = array.clone()

      val start: Long = System.nanoTime()

      TASK match {
        case 1 => calcConst()
        case 2 => calcSum(array)
        case 3 => calcProduct(array)
        case 4 => calcP(array)
        case 5 => calcPByHorner(array)
        case 6 => bubbleSort(array)
        case 7 => quickSort(array2, 0, array2.length - 1)
        case 8 => timSort(array3)
      }

      val end: Long = System.nanoTime()

      val res = end - start

      approx(n - 1) += res

      println(n)
    }

    plotGraph(approx)
  }

  def generateVector(n: Int): Array[Double] = Array.fill(n)(Random.nextInt(MAX))

  def calcConst(): Int = 2 + 2

  def calcSum(array: Array[Double]): Double = array.sum

  def calcProduct(array: Array[Double]): Double = array.product

  def calcP(array: Array[Double]): Double = {
    var res: Double = 0

    for (i <- array.indices) res += (array(i) * Math.pow(1.5, array.length - i - 1))

    res
  }

  def calcPByHorner(array: Array[Double]): Double = array.reduce(_ * 1.5 + _)

  def bubbleSort(array: Array[Double]): Unit = {
    for {
      i <- 0 until array.length - 1
      j <- 0 until array.length - 1 - i if array(j) > array(j + 1)
    } {
      array(j) = array(j) + array(j + 1)
      array(j + 1) = array(j) - array(j + 1)
      array(j) = array(j) - array(j + 1)
    }
  }


  def quickSort(array: Array[Double], from: Int, to: Int): Unit = {
    def swap(i: Int, j: Int): Unit = {
      if (i == j) return

      array(i) = array(i) + array(j)
      array(j) = array(i) - array(j)
      array(i) = array(i) - array(j)
    }

    def partition(from: Int, to: Int, sep: Int): Int = {
      swap(sep, from)

      var j = from
      for (i <- (from + 1) to to if (array(i) < array(from))) {
        j += 1; swap(j, i)
      }
      swap(from, j)
      j
    }

    if (from < to) {
      val sep = partition(from, to, from + (to - from) / 2)
      quickSort(array, from, sep - 1)
      quickSort(array, sep + 1, to)
    }
  }

  def timSort(array: Array[Double]): Unit = java.util.Arrays.sort(array) // timSort is used by default

  def plotGraph(array: Array[Long]): Unit = {
    var approxSeq: Seq[Map[String, Long]] = Seq()

    for (n <- 1 to 2000) {
      if (n > 1 && array(n - 1) < array(n - 2) * 2) {
        approxSeq = approxSeq :+ Map("n" -> n.toLong, "time (nanos)" -> array(n - 1) / 5)
      }
    }

    println("100: " + array(99) / 5)
    println("500: " + array(499) / 5)
    println("2000: " + array(1999) / 5)

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
