package task4.neldermead

import task4.Optimizier
import task4.Point

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
}
