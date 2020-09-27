package task3

import scala.util.Random

object Task {
  val E = 0.001
  val D = 0.0001

  val X_NUM = 1000

  val alpha = Random.nextDouble()
  val beta = Random.nextDouble()

  val x: Array[Double] = (for (k <- 0 to X_NUM) yield (k / X_NUM) + 0.0001).toArray
  val y: Array[Double] = (for (k <- 0 to X_NUM) yield alpha * x(k) + beta + Random.nextGaussian()).toArray

  object Functions {
    def generateY(alpha: Double, beta: Double): Array[Double] = {
      (for (k <- 0 to X_NUM) yield alpha * x(k) + beta + Random.nextGaussian()).toArray
    }

    def generateX(k: Double): Double = k / X_NUM

    def linearApprox(x: Double, a: Double, b: Double): Double = a * x + b

    def rationalApprox(x: Double, a: Double, b: Double): Double = a / (1 + b * x)
  }

  def main(args: Array[String]): Unit = {

  }
}
