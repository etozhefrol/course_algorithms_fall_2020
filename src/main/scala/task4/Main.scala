package task4

import scala.util.Random
import task4.neldermead.NelderMead

object Main {
  val EPS = 0.001
  val MAX_ITER_NUM = 1000
  val X_NUM = 1000

  val x: Array[Double] = (for (k <- 0 to X_NUM) yield 3.0 * k / X_NUM).toArray

  val y: Array[Double] = (for (k <- 0 to X_NUM) yield Random.nextGaussian() + (x(k) match {
    case smaller if x(k) < -100 => -100
    case greater if x(k) > 100 => 100
    case _ => f(x(k))
  })).toArray

  def f(x: Double): Double =
    1 / (Math.pow(x, 2) - 3 * x + 2)

  def F(x: Double, a: Double, b: Double, c: Double, d: Double): Double =
    (a * x + b) / (Math.pow(x, 2) + c * x + d)

  def D(a: Double, b: Double, c: Double, d: Double): Double =
    (for (k <- 0 to X_NUM) yield Math.pow(F(x(k), a, b, c, d) - y(k), 2)).sum

  def D(point: Point): Double = D(point.a, point.b, point.c, point.d)

  def main(args: Array[String]): Unit = {
    val res = NelderMead.minimize(EPS, 1000000)(D)
    println(res)
    println(" ")
  }
}
