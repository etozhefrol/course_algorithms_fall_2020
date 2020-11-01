package task4

trait Optimizier {
  def minimize(eps: Double, maxN: Int)(f: Array[Double] => Array[Double]): Array[Double]
}
