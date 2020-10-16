package task4.neldermead

import task4.Optimizier
import task4.Point

import scala.util.Random

object NelderMead extends Optimizier {
  val ALPHA = 1.0
  val BETA = 0.5
  val GAMMA = 2.0

  override def minimize(eps: Double, iterNum: Long)(f: Point => Double): Point = {
    var h = Point.initPointWithF(f)
    var g = Point.initPointWithF(f)
    var l = Point.initPointWithF(f)
    var c = Point.initPointWithF(f)
    var r = Point.initPointWithF(f)
    var e = Point.initPointWithF(f)

    var res = Point.initPointWithF(f)

    def smth(): Unit = {
      def shrink(xi: Point): Point = l._1 + (xi - l._1) / 2

      val s = Point.calcPointWithF(h._1 * BETA + (c._1 * (1 - BETA)))(f)

      if (s._2 < h._2) {
        h = s
      } else {
        h = Point.calcPointWithF(shrink(h._1))(f)
        g = Point.calcPointWithF(shrink(g._1))(f)
      }
    }

    var points: List[(Point, Double)] = List(Point.randPointWithF(10)(f), Point.randPointWithF(10)(f), Point.randPointWithF(10)(f)).sortBy(_._2)

    var thisSum = Double.MaxValue / 2
    var lastSum = Double.MaxValue

    var k = 0

    while (lastSum - thisSum > eps || k < iterNum) {
      lastSum = thisSum

      h = points(2)
      g = points(1)
      l = points(0)

      c = Point.calcPointWithF((g._1 + l._1) / 2)(f)

      r = Point.calcPointWithF(c._1 * (1 + ALPHA) - (h._1 * ALPHA))(f)

      if (r._2 < l._2) {
        e = Point.calcPointWithF(c._1 * (1 - GAMMA) + (r._1 * GAMMA))(f)

        if (e._2 < r._2) {
          h = e
        } else if (r._2 < e._2) {
          h = r
        }
      } else if (l._2 < r._2 && r._2 < g._2) {
        h = r
      } else if (g._2 < r._2 && r._2 < h._2) {
        val temp = r
        r = h
        h = temp

        smth()
      }

      if (h._2 < r._2) {
        smth()
      }

      // as a result, fl < fg < fh < fr
      points = List(l, g, h).sortBy(_._2)

      thisSum = points(0)._2 + points(1)._2 + points(2)._2

      res = points.head

      k += 1
    }

    res._1
  }

  def minimize2(eps: Double, iterNum: Long)(f: Point => Double): Point = {
    val n = 4
    val k = 10

//    var testPoints: Array[(Point, Double)] = (1 to n + 1).map(_ => Point.randPointWithF(k)(f)).toArray.sortBy(-_._2)

    var testPoints: Array[(Point, Double)] = Array(
      Point.calcPointWithF(Point(0 + Random.nextGaussian() * 0.1, 1 + Random.nextGaussian() * 0.1, -3 + Random.nextGaussian() * 0.1, 2 + Random.nextGaussian() * 0.1))(f),
      Point.calcPointWithF(Point(0 + Random.nextGaussian() * 0.1, 1 + Random.nextGaussian() * 0.1, -3 + Random.nextGaussian() * 0.1, 2 + Random.nextGaussian() * 0.1))(f),
      Point.calcPointWithF(Point(0 + Random.nextGaussian() * 0.1, 1 + Random.nextGaussian() * 0.1, -3 + Random.nextGaussian() * 0.1, 2 + Random.nextGaussian() * 0.1))(f),
      Point.calcPointWithF(Point(0 + Random.nextGaussian() * 0.1, 1 + Random.nextGaussian() * 0.1, -3 + Random.nextGaussian() * 0.1, 2 + Random.nextGaussian() * 0.1))(f),
      Point.calcPointWithF(Point(0 + Random.nextGaussian() * 0.1, 1 + Random.nextGaussian() * 0.1, -3 + Random.nextGaussian() * 0.1, 2 + Random.nextGaussian() * 0.1))(f)
    ).sortBy(-_._2)

    println(testPoints(0)._2.toInt + " is init max - " + testPoints(1)._1)
    println(testPoints(n)._2.toInt + " is init min - " + testPoints(n)._1)

    var i = 0

    while (Math.abs(testPoints(0)._2 - testPoints(n)._2) > eps && i < iterNum) {
      testPoints = testPoints.sortBy(-_._2)

      var h = testPoints(0)
      val g = testPoints(1)
      val l = testPoints(n)

      val c = testPoints.drop(1).map(_._1).fold(Point.initPoint())(_ + _) / n

      var r = Point.calcPointWithF(c * (1 + ALPHA) - (h._1 * ALPHA))(f)

      if (r._2 < l._2) {
        val e = Point.calcPointWithF(c * (1 - GAMMA) + (r._1 * GAMMA))(f)

        if (e._2 <= r._2) {
          h = e
        } else {
          h = r
        }
      } else if (l._2 < r._2 && r._2 < g._2) {
        h = r
      } else if ((g._2 < r._2 && r._2 <= h._2) || (h._2 < r._2)) {
        if (r._2 <= h._2) {
          val temp = r
          r = h
          h = temp
        }

        val s = Point.calcPointWithF(h._1 * BETA + (c * (1 - BETA)))(f)

        if (s._2 <= h._2) {
          h = s
        } else {
          testPoints(0) = h
          testPoints = testPoints.dropRight(1).map(p => Point.calcPointWithF(l._1 + (p._1 - l._1) / 2)(f)) :+ testPoints(n)
        }
      }

      testPoints(0) = h
      testPoints(1) = g
      testPoints(n) = l
      i += 1
    }

    testPoints = testPoints.sortBy(-_._2)
    println(testPoints(n)._2.toInt + " is result min - " + testPoints(n)._1)
    println(f(Point(0, 1, -3, 2)) + " is default")
    println(i + " iters")
    testPoints(n)._1
  }
}
