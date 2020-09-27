package task2_2and3

import com.github.bruneli.scalaopt.core.{DataPoint, MaxIterException, SeqDataSetConverter, SimpleMSEFunction, Variables}
import com.github.bruneli.scalaopt.core.gradient.{BFGS, BFGSConfig, CGConfig, ConjugateGradient}
import com.github.bruneli.scalaopt.core.leastsquares.{LevenbergMarquardt, LevenbergMarquardtConfig}
import vegas.DSL.UnitSpecBuilder
import vegas.{Layer, Line, Nominal, Point, Quant, Scale, Vegas}
import SeqDataSetConverter._
import task2_2and3.Optimizers.{ExhaustiveSearch, GaussMethod, GradientDescent, NelderMeadMethod}

import scala.util.{Random, Try}

object Task {
  val E = 0.001
  val D = 0.0001

  val X_NUM = 100

  val alpha = Random.nextDouble()
  val beta = Random.nextDouble()

  val x: Array[Double] = (for (k <- 0 to X_NUM) yield (k / X_NUM.toDouble)).toArray
  val y: Array[Double] = (for (k <- 0 to X_NUM) yield alpha * x(k) + beta + Random.nextGaussian()).toArray

  def main(args: Array[String]): Unit = {
    import Functions._

    println((alpha, beta))

    val initA = Random.nextDouble()
    val initB = Random.nextDouble()

    val f: (Double, Double, Double) => Double = rationalApprox

    var now: Long = System.nanoTime()

    val gradientDescent = GradientDescent.minimize(
      Array(initA, initB), 0.1, E
    )(array => leastSquares(y, array(0), array(1))(f))

    var end: Long = System.nanoTime()
    println(end - now + " " + gradientDescent.toList)

    var yes1 = true
    var conjugateGradient: Try[Variables] = null
    while (yes1) {
      try {
        now = System.nanoTime()

        conjugateGradient = ConjugateGradient.minimize(
          (x: Variables) => leastSquares(y, x(0), x(1))(f),
          Vector(Random.nextDouble(), Random.nextDouble())
        )(CGConfig(maxIterZoom = 50, tol = E, method = "PR"))

        yes1 = false

        end = System.nanoTime()
        println(end - now + " " + conjugateGradient)
      } catch {
        case ignore: MaxIterException =>
      }
    }

    var yes2 = true
    var newtonMethod: Try[Variables] = null
    while (yes2) {
      try {
        now = System.nanoTime()

        newtonMethod = BFGS.minimize(
          (x: Variables) => leastSquares(y, x(0), x(1))(f),
          Vector(Random.nextDouble(), Random.nextDouble())
        )(BFGSConfig(maxIterZoom = 20, tol = E, c1 = E, c2 = E, c3 = E))

        yes2 = false

        end = System.nanoTime()
        println(end - now + " " + newtonMethod)
      } catch {
        case ignore: MaxIterException =>
      }
    }

    now = System.nanoTime()

    val levenbergMarquardt = LevenbergMarquardt.minimize(
      SimpleMSEFunction(
        (x: Variables, t: Variables) => Seq(f(t(0), x(0), x(1))),
        for (k <- 0 to X_NUM) yield DataPoint(x(k) + 0.00001, y(k))),
      Vector(initA, initB)
    )(new LevenbergMarquardtConfig(stepBound = 0.1))

    end = System.nanoTime()
    println(end - now + " " + levenbergMarquardt)
    now = System.nanoTime()

    val exhaustiveSearch = ExhaustiveSearch.minimize(E)((a, b) => leastSquares(y, a, b)(f))

    end = System.nanoTime()
    println(end - now + " " + exhaustiveSearch)
    now = System.nanoTime()

    val gaussMethod = GaussMethod.minimize(E, (initA, initB))((a, b) => leastSquares(y, a, b)(f))

    end = System.nanoTime()
    println(end - now + " " + gaussMethod)
    now = System.nanoTime()

    val nelderMeadMethod = NelderMeadMethod.minimize(E)((a, b) => leastSquares(y, a, b)(f))

    end = System.nanoTime()
    println(end - now + " " + nelderMeadMethod)

    Plotter.plotGraph(y, Seq(
      ((gradientDescent(0), gradientDescent(1)), "Gradient Descent"),
      ((conjugateGradient.get(0), conjugateGradient.get(1)), "Conjugate Gradient Descent"),
      ((newtonMethod.get(0), newtonMethod.get(1)), "Newton’s method"),
      ((levenbergMarquardt.get(0), levenbergMarquardt.get(1)), "Levenberg-Marquardt"),
      ((exhaustiveSearch._1, exhaustiveSearch._2), "Exhaustive search"),
      ((gaussMethod._1, gaussMethod._2), "Gauss"),
      ((nelderMeadMethod._1, nelderMeadMethod._2), "Nelder-Mead")
    ), linearApprox)
  }

  object Functions {
    def linearApprox(x: Double, a: Double, b: Double): Double = a * x + b

    def rationalApprox(x: Double, a: Double, b: Double): Double = a / (1 + b * x)

    def leastSquares(y: Array[Double], a: Double, b: Double)(f: (Double, Double, Double) => Double): Double = {
      var res: Double = 0.0

      for (k <- 0 to X_NUM) res += Math.pow(f(x(k), a, b) - y(k), 2)

      res
    }
  }

  object Plotter {
    def plotGraph(y: Array[Double], approximations: Seq[((Double, Double), String)],
                  f: (Double, Double, Double) => Double): Unit = {
      val colors = List("#47FF62", "#FF49FB", "#5672FF", "#FF5238", "#38C6FF", "#AC30FF", "#FF1500", "#404040")

      def round(d: Double): Double = BigDecimal(d).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble

      var points: Seq[Map[String, Any]] = Seq()

      for (k <- 0 to X_NUM) {
        points = points :+ Map("x" -> x(k), "y" -> y(k), "color" -> s"generated points by ${round(alpha)} * x + ${round(beta)} + N(0, 1)")
      }

      val graphs: Seq[UnitSpecBuilder] = Layer()
        .withData(points)
        .mark(Point)
        .encodeX("x", Quant)
        .encodeY("y", Quant)
        .encodeColor(field = "color", Nominal, scale = Scale(rangeNominals = colors)
        ) +: approximations
        .map(aprrox => Seq(
            Map("x" -> 0.0, "y" -> f(0.0, aprrox._1._1, aprrox._1._2), "color" -> s"${round(aprrox._1._1)} * x + ${round(aprrox._1._2)} (${aprrox._2})"),
            Map("x" -> 1.0, "y" -> f(1.0, aprrox._1._1, aprrox._1._2), "color" -> s"${round(aprrox._1._1)} * x + ${round(aprrox._1._2)} (${aprrox._2})")
        )).map(
          Layer()
            .withData(_)
            .mark(Line)
            .encodeX("x", Quant)
            .encodeY("y", Quant)
            .encodeColor(field = "color", Nominal, scale = Scale(rangeNominals = colors))
        )

      Vegas.layered("algs", width = 1000, height = 1000)
        .withLayers(graphs: _*)
        .show
    }
  }
}

object Optimizers {
  object ExhaustiveSearch {
    def minimize(eps : Double)(f: (Double, Double) => Double): (Double, Double) = {
      var minSqrs = Double.MaxValue
      var minVals = (Double.MaxValue, Double.MaxValue)

      for {
        a <- 0.0 to 1.0 by eps
        b <- 0.0 to 1.0 by eps
      } {
        val thisSqrs = f(a, b)

        if (thisSqrs < minSqrs) {
          minSqrs = thisSqrs
          minVals = (a, b)
        }
      }

      minVals
    }
  }

  object GaussMethod {
    def minimize(eps : Double, start: (Double, Double))(f: (Double, Double) => Double): (Double, Double) = {
      var minVals = start
      var iteratingFirst = true
      var diff = Double.MaxValue

      while (diff > eps) {
        var minSqrs = Double.MaxValue

        if (iteratingFirst) {
          var minA = 0.0

          for (a <- 0.0 to 1.0 by eps) {
            val thisSqrs = f(a, minVals._2)

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

          for (b <- 0.0 to 1.0 by eps) {
            val thisSqrs = f(minVals._1, b)

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
  }

  object NelderMeadMethod {
    def minimize(eps : Double)(f: (Double, Double) => Double): (Double, Double) = {
      case class Point(a: Double, b: Double) {
        def +(other: Point): Point = Point(a + other.a, b + other.b)

        def -(other: Point): Point = Point(a - other.a, b - other.b)

        def *(const: Double): Point = Point(a * const, b * const)

        def /(const: Double): Point = Point(a / const, b / const)
      }

      def _f(point: Point): Double = f(point.a, point.b)

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
  }

  object GradientDescent {
    def minimize(x : Array[Double], learningRate : Double, tolerance : Double)
                (g: Array[Double] => Double): Array[Double] = {
      val n = x.size
      var h = tolerance
      var alpha = learningRate
      var g0 = g(x)

      var fi = gradG(x,h)(g)

      var delG = 0.0
      for (i <- 0 until n by 1)  delG += fi(i) * fi(i)
      delG = math.sqrt(delG)
      var b = alpha / delG

      while(delG > tolerance){
        for (i <- 0 until n by 1) x(i) -= b * fi(i)
        h /= 2

        fi = gradG(x,h)(g)

        delG = 0.0
        for (i <- 0 until n by 1) delG += fi(i) * fi(i)
        delG = math.sqrt(delG)
        b = alpha / delG

        val g1 = g(x)

        if(g1 > g0) alpha = alpha / 2
        else g0 = g1
      }

      x
    }

    def gradG(x : Array[Double], h : Double)
             (g: Array[Double] => Double): Array[Double] = {
      val n = x.size
      val z : Array[Double] = Array.fill(n){0}
      val y = x
      val g0 = g(x)

      for(i <- 0 until n by 1){
        y(i) += h
        z(i) = (g(y) - g0) / h
      }
      z
    }
  }
}
