package task4.levenbergmarquardt

import com.github.bruneli.scalaopt.core.leastsquares.{LevenbergMarquardt, LevenbergMarquardtConfig}
import com.github.bruneli.scalaopt.core.{DataPoint, DataSet, SeqDataSetConverter, SimpleMSEFunction, Variables}
import opt.multivariate.unconstrained.leastsquares.LevenbergMarquardtAlgorithm
import SeqDataSetConverter._

import scala.compat.java8.FunctionConverters.enrichAsJavaFunction

object LevenbergMarquardt {
  def minimize(eps: Double, maxN: Int, initGuess: Array[Double])(x: Array[Double], y: Array[Double])(f: (Double, Double, Double, Double, Double) => Double): Array[Double] = {
    val levenbergMarquardt = com.github.bruneli.scalaopt.core.leastsquares.LevenbergMarquardt.minimize(
      SimpleMSEFunction(
        (x: Variables, t: Variables) => Seq(f(t.head, x.head, x(1), x(2), x(3))),
        for (k <- x.indices) yield DataPoint(x(k) + 0.0000001, y(k))
      ),
      initGuess.toVector
    )(new LevenbergMarquardtConfig(maxIter = maxN, tol = eps))

    levenbergMarquardt.get.toArray
  }
}
