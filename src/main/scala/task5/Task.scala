package task5

import scala.collection.mutable
import scala.util.Random

object Task {
  val V_NUM = 100
  val E_NUM = 200

  val SEVERAL = 3

  def main(args: Array[String]): Unit = {
    val adjMatrix = genAdjMatrix(V_NUM, E_NUM) // generate matrix

    println("Adjacency matrix:")
    for (row <- 0 to SEVERAL) {
      println(adjMatrix(row).mkString(" "))
    }
    println("...")

    val adjList = getAdjList(adjMatrix)

    println("Adjacency list:")
    for (row <- 0 to SEVERAL) {
      println(row + ": " + adjList(row).mkString(" "))
    }
    println("...")

    println("Components:")
    connectedComponents(getAdjList(adjMatrix))

    println("Shortest path:")
    val from = Random.nextInt(V_NUM)
    var to = Random.nextInt(V_NUM); while (from == to) to = Random.nextInt(V_NUM)
    println("from: " + from)
    println("to: " + to)

    shortestPath(adjList, from, to)

    for (row <- adjMatrix) {
      println(row.mkString(" "))
    }
  }

  def genAdjMatrix(V: Int, E: Int): Array[Array[Int]] = {
    val adjMatrix: Array[Array[Int]] = Array.fill(V_NUM, V_NUM)(0)

    for (n <- 1 to E) {
      var i = Random.nextInt(V)
      var j = Random.nextInt(V)

      while (adjMatrix(i)(j) == 1) {
        i = Random.nextInt(V)
        j = Random.nextInt(V)
      }

      adjMatrix(i)(j) = 1
      adjMatrix(j)(i) = 1
    }

    adjMatrix
  }

  def getAdjList(adjMatrix: Array[Array[Int]]): Array[Array[Int]] =
    adjMatrix.map(col => col.zipWithIndex.filter(_._1 == 1).map(_._2))

  def connectedComponents(adjList: Array[Array[Int]]): Unit = {
    var counter: Int = 0

    val visited: Array[Boolean] = Array.fill(V_NUM)(false)

    def DFS(v: Int): Unit = {
      var localCounter: Int = 0

      visited(v) = true
      print(v + " ")

      for (u <- adjList(v)) {
        if (!visited(u)) {
          DFS(u)
        }
        localCounter += 1
      }

      counter += localCounter
    }

    var i = 1
    for (v <- adjList.indices) {
      if (!visited(v)) {
        print(i + ": ")
        DFS(v)
        println()
        i += 1
      } else {
        counter += 1
      }
    }

    println("iterations: " + counter)
  }

  def shortestPath(adjList: Array[Array[Int]], from: Int, to: Int): Unit = {
    var counter: Int = 0

    val pred: Array[Int] = Array.fill(V_NUM)(-1)
    val dist: Array[Int] = Array.fill(V_NUM)(Integer.MAX_VALUE)

    def BFS(): Boolean = {
      val queue: mutable.Queue[Int] = mutable.Queue()

      val visited: Array[Boolean] = Array.fill(V_NUM)(false)

      visited(from) = true
      dist(from) = 0
      queue.enqueue(from)

      while (queue.nonEmpty) {
        val u = queue.dequeue()
        var localCounter = 1

        for (v <- adjList(u)) {
          if (!visited(v)) {
            visited(v) = true
            dist(v) = dist(u) + 1
            pred(v) = u
            queue.enqueue(v)

            if (v == to) {
              counter += localCounter
              return true
            }
          }

          localCounter += 1
        }

        counter += localCounter
      }

      false
    }

    if (!BFS) {
      println("not connected")
    } else {
      val path: mutable.Set[Int] = mutable.Set()

      var crawl = to
      path.add(crawl)

      while (pred(crawl) != -1) {
        path.add(pred(crawl))
        crawl = pred(crawl)

        counter += 1
      }

      println("shortest length: " + dist(to))
      println("path contains: " + path.mkString(" "))
    }

    println("iterations: " + counter)
  }
}


