package task4

trait Optimizier {
  def minimize(eps: Double, maxN: Long)(f: Point => Double): Point
}
