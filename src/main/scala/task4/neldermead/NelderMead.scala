package task4.neldermead

import task4.Optimizier

import scala.util.Random

object NelderMead {
  val ALPHA = 1.0
  val BETA = 0.5
  val GAMMA = 2.0

  type Point = (Array[Double], Double)

  def minimize(eps: Double, iterNum: Long, init: Array[Double])(f: Array[Double] => Double): Array[Double] = {
    val n = 4
    val k = 10

//    var testPoints: Array[(Point, Double)] = (1 to n + 1).map(_ => Point.randPointWithF(k)(f)).toArray.sortBy(-_._2)

    def getPointWithF(point: Array[Double]): Point =
      (point, f(point))

    var testPoints: Array[Point] = Array(
      getPointWithF(init.map(_ + Random.nextGaussian())),
      getPointWithF(init.map(_ + Random.nextGaussian())),
      getPointWithF(init.map(_ + Random.nextGaussian())),
      getPointWithF(init.map(_ + Random.nextGaussian())),
      getPointWithF(init.map(_ + Random.nextGaussian()))
    ).sortBy(-_._2)

    var i = 0

//    while (Math.abs(testPoints(0)._2 - testPoints(n)._2) > eps && i < iterNum) {
    while (testPoints.map(_._2).sum > eps && i < iterNum) {
      testPoints = testPoints.sortBy(-_._2)

      var h: Point = testPoints(0)
      val g: Point = testPoints(1)
      val l: Point = testPoints(n)

      val c: Array[Double] = testPoints.drop(1)
        .map(_._1)
        .fold(Array(0.0, 0.0, 0.0, 0.0))(_.zip(_).map(e => e._1 + e._2))
        .map(_ / n)

      var r: Point = getPointWithF(c.zip(h._1).map(x => (1 + ALPHA) * x._1 - ALPHA * x._2)) // (xc, xr)

      if (r._2 < l._2) {
        var e: Point = getPointWithF(c.zip(r._1).map(x => (1 - GAMMA) * x._1 + GAMMA * x._2)) // (xe, xr)

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

        var s: Point = getPointWithF(h._1.zip(c).map(x => BETA * x._1 + (1 - BETA) * x._2)) // (xh, xc)

        if (s._2 <= h._2) {
          h = s
        } else {
          testPoints(0) = h
          testPoints = testPoints.dropRight(1).map(i => getPointWithF(l._1.zip(i._1).map(x => x._1 + (x._2 - x._1) / 2))) :+ testPoints(n)
        }
      }

      testPoints(0) = h
      testPoints(1) = g
      testPoints(n) = l
      i += 1
    }

    testPoints = testPoints.sortBy(-_._2)
    testPoints(n)._1
  }
}
