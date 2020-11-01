package task4.particleswarm

import opt.multivariate.unconstrained.order0.direct.NelderMeadAlgorithm
import opt.multivariate.unconstrained.order0.evol.CcPsoAlgorithm
import opt.multivariate.unconstrained.order0.evol.AdaptivePsoAlgorithm

import scala.compat.java8.FunctionConverters.enrichAsJavaFunction

object ParticleSwarmOptimization {
  def minimize(eps: Double, maxN: Int, initGuess: Array[Double])(f: Array[Double] => java.lang.Double): Array[Double] = {
    val minimizier: AdaptivePsoAlgorithm = new AdaptivePsoAlgorithm(eps, eps, maxN, 100)

    val res = minimizier.optimize(f.asJava, initGuess)

    res.getOptimalPoint
  }
}
