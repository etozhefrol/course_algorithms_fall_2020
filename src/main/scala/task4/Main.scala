package task4

import task4.differentialevolution.DifferentialEvolution
import task4.levenbergmarquardt.LevenbergMarquardt
import task4.neldermead._
import task4.particleswarm.ParticleSwarmOptimization

import scala.util.Random
import vegas.DSL.UnitSpecBuilder
import vegas.{Area, Layer, Line, Nominal, Point, Quant, Scale, Vegas}

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

  def D(p: Array[Double]): Double =
    (for (k <- 0 to X_NUM) yield Math.pow(F(x(k), p(0), p(1), p(2), p(3)) - y(k), 2)).sum

  def D2(p: Array[Double]): Array[Double] =
    (for (k <- (0 to X_NUM).toArray) yield Math.pow(F(x(k), p(0), p(1), p(2), p(3)) - y(k), 2))

  def main(args: Array[String]): Unit = {
    val initialGuess: Array[Double] = Array(1, 1, 1, 1)
//
    var now = System.nanoTime()
    val levenbergMarquardt = LevenbergMarquardt.minimize(EPS, MAX_ITER_NUM, initialGuess)(x, y)(F)
    println(System.nanoTime() - now)

    now = System.nanoTime()
    val differentialEvolution = DifferentialEvolution.minimize(EPS, MAX_ITER_NUM, initialGuess)(D)
    println(System.nanoTime() - now)

    now = System.nanoTime()
    val nelderMead = NelderMead.minimize(EPS, MAX_ITER_NUM, initialGuess)(D)
    println(System.nanoTime() - now)

    now = System.nanoTime()
    val particleSwarmOptimization = ParticleSwarmOptimization.minimize(EPS, MAX_ITER_NUM, initialGuess)(D)
    println(System.nanoTime() - now)

    val res = Array(
      (levenbergMarquardt, "LevenbergMarquardt"),
      (differentialEvolution, "DifferentialEvolution"),
      (nelderMead, "NelderMead"),
      (particleSwarmOptimization, "ParticleSwarmOptimization")
    )

    println("0, 1, -3, 2")

    Plotter.plotGraph(res)
  }

  object Plotter {
    def plotGraph(approximations: Array[(Array[Double], String)]): Unit = {
      val colors = List("#47FF62", "#FF49FB", "#5672FF", "#FF5238", "#38C6FF", "#AC30FF", "#FF1500", "#404040")

      def round(d: Double): Double = BigDecimal(d).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble

      var points: Seq[Map[String, Any]] = Seq()

      def putPoint(y: Double): Double = y match {
        case less if y < -100 => -100
        case more if y > 100 => 100
        case _ => y
      }

      for (k <- 0 to X_NUM) {
        points = points :+ Map("x" -> x(k), "y" -> putPoint(y(k)), "color" -> s"generated points")
      }

      def getLine(res: Array[Double], name: String): Seq[Map[String, Any]] = {
        for (k <- 0 to X_NUM) yield Map(
          "x" -> x(k),
          "y" -> putPoint(F(x(k), res(0), res(1), res(2), res(3))),
          "color" -> s"(${round(res(0))} * x + ${round(res(1))}) / (x ^ 2 + ${round(res(2))} * x + ${round(res(3))}) ($name)"
        )
      }

      val graphs: Seq[UnitSpecBuilder] = Layer()
        .withData(points)
        .mark(Point)
        .encodeX("x", Quant)
        .encodeY("y", Quant)
        .encodeColor(field = "color", Nominal, scale = Scale(rangeNominals = colors)
        ) +: approximations
        .map(aprrox => getLine(aprrox._1, aprrox._2))
        .map(Layer()
          .withData(_)
          .mark(Line)
          .encodeX("x", Quant)
          .encodeY("y", Quant)
          .encodeColor(field = "color", Nominal, scale = Scale(rangeNominals = colors)))

      Vegas.layered("algs", width = 1000, height = 1000)
        .withLayers(graphs: _*)
        .show
    }
  }
}