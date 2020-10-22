package task6

import scala.util.Random

object Task {
  def main(args: Array[String]): Unit = {
    Matrix.genAdjMatrix()
    val rand = Random.nextInt(Matrix.V_NUM)
    val a = Matrix.dijkstra(0)
    val b = Matrix.bellmanFord(0)
    for (i <- 0 until Matrix.V_NUM) println(s"$i: ${a(i)} vs ${b(i)}")

    for (n <- 0 until 10) {
      val dStart = System.nanoTime()
      Matrix.dijkstra(rand)
      val dRes = System.nanoTime() - dStart
      println(s"dijkstra: $dRes nanos")

      val bStart = System.nanoTime()
      Matrix.bellmanFord(rand)
      val bRes = System.nanoTime() - bStart
      println(s"bellmanF: $bRes nanos")
    }

  }
}

object Matrix {
  val V_NUM = 100
  val E_NUM = 500
  val MAX_WEIGHT = 100

  var adjMatrix: Array[Array[Int]] = Array.fill(V_NUM, V_NUM)(0)
  var graph: Array[(Int, Int, Int)] = Array()

  def genAdjMatrix(): Unit = {
    for (n <- 1 to E_NUM) {
      var i = Random.nextInt(V_NUM)
      var j = Random.nextInt(V_NUM)

      while (adjMatrix(i)(j) > 0) {
        i = Random.nextInt(V_NUM)
        j = Random.nextInt(V_NUM)
      }

      val weight = Random.nextInt(MAX_WEIGHT) + 1
      adjMatrix(i)(j) = weight
      adjMatrix(j)(i) = weight
    }

    graph = (for {
      i <- 0 until V_NUM
      j <- 0 until V_NUM if adjMatrix(i)(j) > 0
    } yield (i, j, adjMatrix(i)(j))).toArray // (from, to, dist)
  }

  def dijkstra(sourceVertex: Int): Array[Int] = {
    val spt: Array[Boolean] = Array.ofDim(V_NUM)
    val distance: Array[Int] = Array.fill(V_NUM)(Int.MaxValue)

    def getMinimumV: Int = {
      var minKey = Int.MaxValue
      var v = -1

      for (i <- 0 until V_NUM if !spt(i) && minKey >= distance(i)) {
        minKey = distance(i)
        v = i
      }
      v
    }

    distance(sourceVertex) = 0

    for (i <- 0 until V_NUM) {
      val u = getMinimumV

      spt(u) = true

      for (v <- 0 until V_NUM if adjMatrix(u)(v) > 0 && !spt(v) && adjMatrix(u)(v) != Int.MaxValue) {
        val newKey = adjMatrix(u)(v) + distance(u)

        if (newKey < distance(v)) distance(v) = newKey
      }
    }

    distance
  }

  def bellmanFord(sourceVertex: Int): Array[Int] = {
    val distance: Array[Int] = Array.fill(V_NUM)(Int.MaxValue / 2)

    distance(sourceVertex) = 0

    for {
      i <- 0 until V_NUM - 1
      j <- graph.indices
    } {
      val u = graph(j)._1
      val v = graph(j)._2
      val weight = graph(j)._3
      if (distance(u) + weight < distance(v))
        distance(v) = distance(u) + weight
    }

    distance
  }
}

object Field {
  val FIELD_SIZE = 100

  val cells: Array[Array[Char]] = Array.fill(FIELD_SIZE)(Array.fill(FIELD_SIZE)(' '))

  val obst: Char = 'x'
  val point: Char = 'O'

  def show(): Unit = {
    for (i <- 0 until FIELD_SIZE) {
      for (j <- 0 until FIELD_SIZE) {
        print(s"${cells(i)(j).toString}|")
      }

      println()

      for (j <- 0 until FIELD_SIZE) {
        print("-+")
      }

      println()
    }
  }
}
