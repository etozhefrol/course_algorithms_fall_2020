package task4

import scala.util.Random

case class Point(a: Double, b: Double, c: Double, d: Double) {
  def +(other: Point): Point = Point(a + other.a, b + other.b, c + other.c, d + other.d)

  def -(other: Point): Point = Point(a - other.a, b - other.b, c - other.c, d - other.d)

  def *(const: Double): Point = Point(a * const, b * const, c * const, d * const)

  def /(const: Double): Point = Point(a / const, b / const, c / const, d / const)

  def sum(): Double = a + b + c + d
}

object Point {
  def initPoint(): Point = Point(0.0, 0.0, 0.0, 0.0)

  def initPointWithF(f: Point => Double): (Point, Double) = (initPoint(), f(initPoint()))

  def calcPointWithF(point: Point)(f: Point => Double): (Point, Double) = (point, f(point))

  def randPoint(k: Double): Point = Point(k * Random.nextDouble(), k * Random.nextDouble(), k * Random.nextDouble(), Random.nextDouble())

  def randPointWithF(k: Double)(f: Point => Double): (Point, Double) = {
    val point: Point = randPoint(k)
    (point, f(point))
  }

  def minByF(point1: (Point, Double), point2: (Point, Double)): (Point, Double) = {
    if(point1._2 < point2._2) point1 else point2
  }
}
