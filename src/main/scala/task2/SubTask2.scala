package task2

import com.github.bruneli.scalaopt.core.{ConfigPars, SimpleFunctionFiniteDiffGradient, Variables}
import com.github.bruneli.scalaopt.core.gradient.{BFGS, BFGSConfig, CGConfig, ConjugateGradient, NewtonCG}
import com.github.bruneli.scalaopt.core.gradient.NewtonCG.defaultConfig
import com.github.bruneli.scalaopt.core._
import com.github.bruneli.scalaopt.core.leastsquares.{LevenbergMarquardt, LevenbergMarquardtConfig}
import SeqDataSetConverter._
import com.github.bruneli.scalaopt.core.leastsquares.LevenbergMarquardt.defaultConfig
import org.apache.commons.math3.fitting.leastsquares.{GaussNewtonOptimizer, LevenbergMarquardtOptimizer}
import vegas.spec.Spec
import vegas.{Area, Bar, Bin, Layer, Legend, Line, Nominal, Point, Quant, Scale, Vegas}

import scala.util.Random

object SubTask2 {
  val eps = 0.001
  val D = 0.0001

  val X_NUM = 100

  def main(args: Array[String]): Unit = {
    val alpha = Random.nextDouble()
    val beta = Random.nextDouble()

    println(alpha)
    println(beta)

    val y = generateY(alpha, beta)

    val function: SimpleFunctionFiniteDiffGradient = new SimpleFunctionFiniteDiffGradient(v => calcSquares(y, v(0), v(1))(f1), new ConfigPars(/*maxIter = Int.MaxValue*/))

    val now1 = System.nanoTime()
    var res1 = exhaustiveSearch(y)(f1)
    println(System.nanoTime() - now1)

    val now2 = System.nanoTime()
    var res2 = gaussMethod(y)(f1)
    println(System.nanoTime() - now2)

    val now3 = System.nanoTime()
    var res3 = nelderMeadMethod(eps, y)(f1)
    println(System.nanoTime() - now3)

    // todo: Visualize the data and the approximants obtained in a plot separately for each type of approximant
    println(res1)
    println(res2)
    println(res3)

    plotGraph(y, alpha, beta, res1._1, res1._2, res2._1, res2._2, res3._1, res3._2)
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
                      (f: (Double, Double, Double) => Double): (Double, Double, (Int, Int)) = {
    var minSqrs = Double.MaxValue
    var minVals = (Double.MaxValue, Double.MaxValue)

    var (it, fi) = (0, 0)

    for {
      a <- 0.0 to 1.0 by eps
      b <- 0.0 to 1.0 by eps
    } {
      val thisSqrs = calcSquares(y, a, b)(f)

      if (thisSqrs < minSqrs) {
        minSqrs = thisSqrs
        minVals = (a, b)
      }

      it = it + 1
      fi = fi + 1
    }

    (minVals._1, minVals._2, (it, fi))
  }

  def gaussMethod(y: Array[Double])
                 (f: (Double, Double, Double) => Double): (Double, Double, (Int, Int)) = {
    var minVals = (Random.nextDouble(), Random.nextDouble())
    var iteratingFirst = true
    var diff = Double.MaxValue

    var (it, fi) = (0, 0)

    while (diff > eps) {
      var minSqrs = Double.MaxValue

      if (iteratingFirst) {
        var minA = 0.0

        for (a <- 0.0 to 1.0 by D) {
          val thisSqrs = calcSquares(y, a, minVals._2)(f)

          if (thisSqrs < minSqrs) {
            minSqrs = thisSqrs
            minA = a
          }

          it = it + 1
          fi = fi + 1
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

          it = it + 1
          fi = fi + 1
        }

        diff = Math.abs(minVals._2 - minB)
        iteratingFirst = true
        minVals = (minVals._1, minB)
      }
    }

    (minVals._1, minVals._2, (it, fi))
  }

  def nelderMeadMethod(eps: Double, y: Array[Double])
                      (f: (Double, Double, Double) => Double): (Double, Double, (Int, Int)) = {
    case class Point(a: Double, b: Double) {
      def +(other: Point): Point = Point(a + other.a, b + other.b)

      def -(other: Point): Point = Point(a - other.a, b - other.b)

      def *(const: Double): Point = Point(a * const, b * const)

      def /(const: Double): Point = Point(a / const, b / const)
    }

    var (it, fi) = (0, 0)

    def _f(point: Point): Double = {
      fi += 1
      calcSquares(y, point.a, point.b)(f)
    }

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

    while (lastSum - thisSum > eps) {
      lastSum = fh + fg + fl

      val pointsToF = startPoints.map(point => (point, _f(point))).sortBy(_._2)

      xh = pointsToF(2)._1;
      fh = pointsToF(2)._2
      xg = pointsToF(1)._1;
      fg = pointsToF(1)._2
      xl = pointsToF(0)._1;
      fl = pointsToF(0)._2

      xc = (xg + xl) / 2

      xr = xc * (1 + alpha) - (xh * alpha);
      fr = _f(xr)

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

        val xs = xh * beta + (xc * (1 - beta));
        val fs = _f(xs)

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

      it = it + 1

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

    (bestPoint.a, bestPoint.b, (it, fi))
  }

  def plotGraph(y: Array[Double], alpha: Double, beta: Double,
                a1: Double, b1: Double, a2: Double, b2: Double, a3: Double, b3: Double): Unit = {
    val colors = List("#47FF62", "#FF49FB", "#5672FF", "#FF5238", "#38C6FF", "#AC30FF", "#FF1500", "#404040")

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
      Map("x" -> 0.0, "y" -> f1(0.0, a1, b1), "color" -> s"${round(a1)} * x + ${round(b1)} (exhaustive)"),
      Map("x" -> 1.0, "y" -> f1(1.0, a1, b1), "color" -> s"${round(a1)} * x + ${round(b1)} (exhaustive)")
    )

    val graph2: Seq[Map[String, Any]] = Seq(
      Map("x" -> 0.0, "y" -> f1(0.0, a2, b2), "color" -> s"${round(a2)} * x + ${round(b2)} (gauss)"),
      Map("x" -> 1.0, "y" -> f1(1.0, a2, b2), "color" -> s"${round(a2)} * x + ${round(b2)} (gauss)")
    )

    val graph3: Seq[Map[String, Any]] = Seq(
      Map("x" -> 0.0, "y" -> f1(0.0, a3, b3), "color" -> s"${round(a3)} * x + ${round(b3)} (nelder-mead)"),
      Map("x" -> 1.0, "y" -> f1(1.0, a3, b3), "color" -> s"${round(a3)} * x + ${round(b3)} (nelder-mead)")
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
        .encodeColor(field = "color", Nominal, scale = Scale(rangeNominals = colors)),
      Layer()
        .withData(graph3)
        .mark(Line)
        .encodeX("x", Quant)
        .encodeY("y", Quant)
        .encodeColor(field = "color", Nominal, scale = Scale(rangeNominals = colors))
    ).show
  }

}
