package task2

import vegas.spec.Spec
import vegas.{Area, Bar, Bin, Layer, Legend, Line, Nominal, Point, Quant, Scale, Vegas}

import scala.util.Random

object SubTask2 {
  val E = 0.001
  val D = 0.0001

  val X_NUM = 100

  def main(args: Array[String]): Unit = {
    val alpha = Random.nextDouble()
    val beta = Random.nextDouble()

    println(alpha)
    println(beta)

    val y = generateY(alpha, beta)

    val res1 = nelderMeadMethod(y)(f1)
    val res2 = nelderMeadMethod(y)(f2)

    println(res1)
    println(res2)
    plotGraph(y, alpha, beta, res1._1, res1._2, res2._1, res2._2)
  }

  def generateY(alpha: Double, beta: Double): Array[Double] = {
    val res = for (k <- 0 to X_NUM) yield alpha * x(k) + beta + Random.nextGaussian()

    res.toArray
  }

  def x(k: Double): Double = k / X_NUM

  def f1(x: Double, a: Double, b: Double): Double = a * x + b

  def f2(x: Double, a: Double, b: Double): Double = a / (1 + b * x)

  def calcSquares(y: Array[Double], a: Double, b: Double)(f: (Double, Double, Double) => Double): Double = {
    var res: Double = 0.0

    for (k <- 0 to X_NUM) res += Math.pow(f(x(k), a, b) - y(k), 2)

    res
  }

  def exhaustiveSearch(y: Array[Double])
                      (f: (Double, Double, Double) => Double): (Double, Double) = {
    var minSqrs = Double.MaxValue
    var minVals = (Double.MaxValue, Double.MaxValue)

    for {
      a <- 0.0 to 1.0 by E
      b <- 0.0 to 1.0 by E
    } {
      val thisSqrs = calcSquares(y, a, b)(f)

      if (thisSqrs < minSqrs) {
        minSqrs = thisSqrs
        minVals = (a, b)
      }
    }

    minVals
  }

  def gaussMethod(y: Array[Double])
                 (f: (Double, Double, Double) => Double): (Double, Double) = {
    var minVals = (Random.nextDouble(), Random.nextDouble())
    var iteratingFirst = true
    var diff = Double.MaxValue

    while (diff > E) {
      var minSqrs = Double.MaxValue

      if (iteratingFirst) {
        var minA = 0.0

        for (a <- 0.0 to 1.0 by D) {
          val thisSqrs = calcSquares(y, a, minVals._2)(f)

          if (thisSqrs < minSqrs) {
            minSqrs = thisSqrs
            minA = a
          }
        }

        diff = Math.abs(minVals._1 - minA)
        iteratingFirst = false
        minVals = (minA, minVals._2)
      } else {
        var minB = 0.0

        for (b <- 0.0 to 1.0 by D) {
          val thisSqrs = calcSquares(y, minVals._1, b)(f)

          if (thisSqrs < minSqrs) {
            minSqrs = thisSqrs
            minB = b
          }
        }

        diff = Math.abs(minVals._2 - minB)
        iteratingFirst = true
        minVals = (minVals._1, minB)
      }
    }

    minVals
  }

  def nelderMeadMethod(y: Array[Double])
                 (f: (Double, Double, Double) => Double): (Double, Double) = {
    case class Point(a: Double, b: Double) {
      def +(other: Point): Point = Point(a + other.a, b + other.b)
      def -(other: Point): Point = Point(a - other.a, b - other.b)
      def *(const: Double): Point = Point(a * const, b * const)
      def /(const: Double): Point = Point(a / const, b / const)
    }

    def _f(point: Point): Double = calcSquares(y, point.a, point.b)(f)

    val alpha = 1
    val beta = 0.5
    val gamma = 2

    var lastSum = Double.MaxValue
    var thisSum = 0.0
    var bestPoint = Point(0.0, 0.0)

    var xh = Point(0.0, 0.0)
    var xg = Point(0.0, 0.0)
    var xl = Point(0.0, 0.0)
    var xc = Point(0.0, 0.0)
    var xr = Point(0.0, 0.0)
    var xe = Point(0.0, 0.0)

    var fh = Double.MaxValue
    var fg = Double.MaxValue
    var fl = Double.MaxValue
    var fr = 0.0
    var fe = Random.nextDouble()

    var startPoints = Array(
      Point(Random.nextDouble(), Random.nextDouble()),
      Point(Random.nextDouble(), Random.nextDouble()),
      Point(Random.nextDouble(), Random.nextDouble())
    )

    while (lastSum - thisSum > E) {
      lastSum = fh + fg + fl

      val pointsToF = startPoints.map(point => (point, _f(point))).sortBy(_._2)

      xh = pointsToF(2)._1; fh = pointsToF(2)._2
      xg = pointsToF(1)._1; fg = pointsToF(1)._2
      xl = pointsToF(0)._1; fl = pointsToF(0)._2

      xc = (xg + xl) / 2

      xr = xc * (1 + alpha) - (xh * alpha); fr = _f(xr)

      if (fr < fl) {
        xe = xc * (1 - gamma) + (xr * gamma)
        fe = _f(xe)

        if (fe < fr) {
          xh = xe
          fh = _f(xh)
        } else if (fr < fe) {
          xh = xr;
          fh = _f(xh)
        }
      } else if (fl < fr && fr < fg) {
        xh = xr
        fh = _f(xh)
      } else if (fg < fr && fr < fh) {
        xr = xr + xh
        xh = xr - xh
        fh = _f(xh)
        xr = xr - xh
        fr = _f(xr)
        step6()
      }

      if (fh < fr) {
        step6()
      }

      // As a result, fl < fg < fh < fr

      def step6(): Unit = {
        def shrink(xi: Point): Point = xl + (xi - xl) / 2

        val xs = xh * beta + (xc * (1 - beta)); val fs = _f(xs)

        if (fs < fh) {
          xh = xs
          fh = _f(xh)
        } else {
          xh = shrink(xh)
          fh = _f(xh)
          xg = shrink(xg)
          fg = _f(xg)
        }
      }

      startPoints = Array(xl, xg, xh)

      thisSum = fh + fg + fl

      if (fh < fg) {
        if (fh < fl) {
          bestPoint = xh
        } else {
          bestPoint = xl
        }
      } else {
        if (fg < fl) {
          bestPoint = xg
        } else {
          bestPoint = xl
        }
      }
    }

    (bestPoint.a, bestPoint.b)
  }

  def plotGraph(y: Array[Double], alpha: Double, beta: Double,
                a1: Double, b1: Double, a2: Double, b2: Double): Unit = {
    val colors = List("#47FF62", "#FF49FB", "#5672FF", "#FF5238")

    def round(d: Double): Double = BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    var points: Seq[Map[String, Any]] = Seq()

    for (k <- 0 to X_NUM) {
      points = points :+ Map("x" -> x(k), "y" -> y(k), "color" -> "generated points")
    }

    val graph0: Seq[Map[String, Any]] = Seq(
      Map("x" -> 0.0, "y" -> f1(0.0, alpha, beta), "color" -> s"${round(alpha)} * x + ${round(beta)} (random)"),
      Map("x" -> 1.0, "y" -> f1(1.0, alpha, beta), "color" -> s"${round(alpha)} * x + ${round(beta)} (random)")
    )

    val graph1: Seq[Map[String, Any]] = Seq(
      Map("x" -> 0.0, "y" -> f1(0.0, a1, b1), "color" -> s"${round(a1)} * x + ${round(b1)} (linear approx)"),
      Map("x" -> 1.0, "y" -> f1(1.0, a1, b1), "color" -> s"${round(a1)} * x + ${round(b1)} (linear approx)")
    )

    val graph2: Seq[Map[String, Any]] = Seq(
      Map("x" -> 0.0, "y" -> f1(0.0, a2, b2), "color" -> s"${round(a2)} * x + ${round(b2)} (rational approx)"),
      Map("x" -> 1.0, "y" -> f1(1.0, a2, b2), "color" -> s"${round(a2)} * x + ${round(b2)} (rational approx)")
    )

    val res = Vegas.layered("algs", width = 500, height = 500).withLayers(
      Layer()
        .withData(points)
        .mark(Point)
        .encodeX("x", Quant)
        .encodeY("y", Quant)
        .encodeColor(field = "color", Nominal, scale = Scale(rangeNominals = colors)),
      Layer()
        .withData(graph0)
        .mark(Line)
        .encodeX("x", Quant)
        .encodeY("y", Quant)
        .encodeColor(field = "color", Nominal, scale = Scale(rangeNominals = colors)),
      Layer()
        .withData(graph1)
        .mark(Line)
        .encodeX("x", Quant)
        .encodeY("y", Quant)
        .encodeColor(field = "color", Nominal, scale = Scale(rangeNominals = colors)),
      Layer()
        .withData(graph2)
        .mark(Line)
        .encodeX("x", Quant)
        .encodeY("y", Quant)
        .encodeColor(field = "color", Nominal, scale = Scale(rangeNominals = colors))
    ).show
  }

}
