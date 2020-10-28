package task2

import vegas.{Layer, Line, Quant, Vegas}

import scala.util.Random

object SubTask1 {
  val E = 0.001

  def main(args: Array[String]): Unit = {
    def runTask(section: (Double, Double))(f: Double => Double): Unit = {
      println(bruteForce(section)(f))
      println(dichotomy(section)(f))
      println(goldenSection(section)(f))
    }

    runTask((0.0, 1.0))(f1)
    runTask((0.0, 1.0))(f2)
    runTask((0.01, 1.0))(f3)
  }

  def f1(x: Double): Double = Math.pow(x, 3)

  def f2(x: Double): Double = if (x > 0.2) x - 0.2 else 0.2 - x

  def f3(x: Double): Double = x * Math.sin(1 / x)

  def bruteForce(section: (Double, Double))(f: Double => Double): (Long, Long, Double) = {
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

    (fIterator, iIterator, res)
  }

  def dichotomy(section: (Double, Double))(f: Double => Double): (Long, Long, (Double, Double)) = {
    var fIterator: Long = 0
    var iIterator: Long = 0

    var p = (section._1, section._2)
    val D = E / 10

    var x1 = 0.0
    var x2 = 0.0

    while (p._2 - p._1 > E) {
      x1 = ((p._1 + p._2) - D) / 2
      x2 = ((p._1 + p._2) + D) / 2

      p = if (f(x1) <= f(x2)) {
        (p._1, x2)
      } else {
        (x1, p._2)
      }

      iIterator += 1
      fIterator += 2
    }

    (fIterator, iIterator, (x1, x2))
  }

  def goldenSection(section: (Double, Double))(f: Double => Double): (Long, Long, (Double, Double)) = {
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

    (fIterator, iIterator, x)
  }
}
