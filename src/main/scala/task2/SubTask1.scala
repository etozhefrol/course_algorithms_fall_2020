package task2

import vegas.{Layer, Line, Quant, Vegas}

import scala.util.Random

object SubTask1 {
  val E = 0.001

  def main(args: Array[String]): Unit = {
    runTask1()
    runTask2()
    runTask3()
  }

  def f1(x: Double): Double = Math.pow(x, 3)

  def f2(x: Double): Double = if (x > 0.2) x - 0.2 else 0.2 - x

  def f3(x: Double): Double = x * Math.sin(1 / x)

  def bruteForce(section: (Double, Double))(f: Double => Double): (Long, Long) = {
    var fIterator: Long = 0
    var iIterator: Long = 0

    var min = Double.MaxValue
    var res = 0.0

    for (point <- section._1 to section._2 by E) {
      if (f(point) < min) {
        res = point
        min = f(point)

        fIterator += 1
      }

      iIterator += 1
      fIterator += 1
    }

    (fIterator, iIterator)
  }

  def dichotomy(section: (Double, Double))(f: Double => Double): (Long, Long) = {
    var fIterator: Long = 0
    var iIterator: Long = 0

    var p = (section._1, section._2)
    val D = E / 10

    while (p._2 - p._1 > E) {
      val x1 = ((p._1 + p._2) - D) / 2
      val x2 = ((p._1 + p._2) + D) / 2

      p = if (f(x1) <= f(x2)) {
        (p._1, x2)
      } else {
        (x1, p._2)
      }

      iIterator += 1
      fIterator += 2
    }

    (fIterator, iIterator)
  }

  def goldenSection(section: (Double, Double))(f: Double => Double): (Long, Long) = {
    var fIterator: Long = 0
    var iIterator: Long = 0

    var p = (section._1, section._2)

    def calcX1(p: (Double, Double)): Double = p._1 + (3 - Math.sqrt(5)) * (p._2 - p._1) / 2
    def calcX2(p: (Double, Double)): Double = p._2 + (Math.sqrt(5) - 3) * (p._2 - p._1) / 2

    var x = (calcX1(p), calcX2(p))

    while (p._2 - p._1 > E) {
      if (f(x._1) <= f(x._2)) {
        p = (p._1, x._2)
        x = (calcX1(p), x._1)
      } else {
        p = (x._1, p._2)
        x = (x._2, calcX2(p))
      }

      iIterator += 1
      fIterator += 2
    }

    (fIterator, iIterator)
  }

  def runTask1(): Unit = {
    println("f1")
    println(bruteForce((0.0, 1.0))(f1))
    println(dichotomy((0.0, 1.0))(f1))
    println(goldenSection((0.0, 1.0))(f1))
  }

  def runTask2(): Unit = {
    println("f2")
    println(bruteForce((0.0, 1.0))(f2))
    println(dichotomy((0.0, 1.0))(f2))
    println(goldenSection((0.0, 1.0))(f2))
  }

  def runTask3(): Unit = {
    println("f3")
    println(bruteForce((0.01, 1.0))(f3))
    println(dichotomy((0.01, 1.0))(f3))
    println(goldenSection((0.01, 1.0))(f3))
  }

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
