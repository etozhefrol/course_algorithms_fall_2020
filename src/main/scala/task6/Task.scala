package task6

import scala.util.Random

object Task {
  def main(args: Array[String]): Unit = {
    for (n <- 0 until 5) {
      II.genField()
      val tStart = System.nanoTime()
      II.run()
      val tFinish = System.nanoTime() - tStart
      println(tFinish)
    }
  }
}

object I {
  val V_NUM = 100
  val E_NUM = 500
  val MAX_WEIGHT = 100

  var adjMatrix: Array[Array[Int]] = Array.fill(V_NUM, V_NUM)(0)
  var graph: Array[(Int, Int, Int)] = Array()

  def run(): Unit = {
    I.genAdjMatrix()
    val rand = Random.nextInt(I.V_NUM)
    val a = I.dijkstra(0)
    val b = I.bellmanFord(0)
    for (i <- 0 until I.V_NUM) println(s"$i: ${a(i)} vs ${b(i)}")

    for (n <- 0 until 10) {
      val dStart = System.nanoTime()
      I.dijkstra(rand)
      val dRes = System.nanoTime() - dStart
      println(s"dijkstra: $dRes nanos")

      val bStart = System.nanoTime()
      I.bellmanFord(rand)
      val bRes = System.nanoTime() - bStart
      println(s"bellmanF: $bRes nanos")
    }
  }

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

object II {
  val FIELD_SIZE = 10
  val OBSTACLE_COUNT = 30
  val LENGTH_STRAIGHT = 10
  val LENGTH_DIAGONAL = 14

  val cEmpty: Char = ' '
  val cObst: Char = 'x'
  val cStart: Char = 'S'
  val cFinish: Char = 'F'
  val cDead: Char = 'd'
  val cCalculated: Char = '.'
  val cCurrent: Char = 'C'
  val cResult: Char = '*'

  var start: Point = Point(0, 0)
  var finish: Point = Point(0, 0)

  case class Point(y: Int, x: Int) {
    def -(other: Point): Int = Math.abs(x - other.x) + Math.abs(y - other.y)
    def equals(other: Point): Boolean = x == other.x && y == other.y
    def ==(other: Point): Boolean = equals(other)
    def !=(other: Point): Boolean = !equals(other)
    def lengthTo(i: Int, j: Int): Double =
      if (x == j || y == i) 1.0 else Math.sqrt(2)

    def calcNeighbours(): Unit  = {
      for {
        i <- Math.max(y - 1, 0) to Math.min(y + 1, FIELD_SIZE - 1)
        j <- Math.max(x - 1, 0) to Math.min(x + 1, FIELD_SIZE - 1)
        if !field(i)(j).isObst && !field(i)(j).isDead && (y != i || x != j)
      } {
        if (field(y)(x).dStart + lengthTo(i, j) < field(i)(j).dStart) {
          field(i)(j).calculate(field(y)(x).dStart + lengthTo(i, j), this)
        }
      }
    }

    def chooseNext(): Point = {
      var res = Point(0, 0)
      var minD = Double.MaxValue

      for {
        i <- 0 until FIELD_SIZE
        j <- 0 until FIELD_SIZE
        if !field(i)(j).isObst && !field(i)(j).isDead && field(i)(j).isCalculated
      } {
        if (field(i)(j).dTotal < minD) {
          res = field(i)(j).point
          minD = field(i)(j).dTotal
        }
      }

      res
    }
  }

  object Point {
    def rand: Point = Point(Random.nextInt(FIELD_SIZE), Random.nextInt(FIELD_SIZE))
  }

  case class Cell(var char: Char, var dStart: Double, point: Point, var from: Point) {
    val dFinish: Int = point - finish

    def dTotal: Double = dStart + dFinish

    def setDead(): Unit = char = cDead
    def setCalculated(): Unit = char = cCalculated
    def setCurrent(): Unit = char = cCurrent
    def setResult(): Unit = char = cResult

    def setParams(char: Char, dStart: Double): Unit = {
      this.char = char
      this.dStart = dStart
    }
    def setParams(dStart: Double): Unit = {
      this.dStart = dStart
    }
    def calculate(dStart: Double, from: Point): Unit = {
      this.dStart = dStart
      this.from = from
      char = cCalculated
    }

    def isEmpty: Boolean = char == cEmpty
    def isObst: Boolean = char == cObst
    def isStart: Boolean = char == cStart
    def isFinish: Boolean = char == cFinish
    def isDead: Boolean = char == cDead
    def isCalculated: Boolean = char == cCalculated
  }

  val field: Array[Array[Cell]] = Array.fill(FIELD_SIZE)(Array.ofDim(FIELD_SIZE))

  def genField(): Unit = {
    start = Point.rand
    finish = Point.rand

    for {
      i <- 0 until FIELD_SIZE
      j <- 0 until FIELD_SIZE
    } {
      field(i)(j) = Cell(cEmpty, Int.MaxValue / 2, Point(i, j), start)
    }

    field(start.y)(start.x).setParams(cStart, 0)
    field(finish.y)(finish.x).setParams(cFinish, Int.MaxValue / 2)

    for (i <- 0 until OBSTACLE_COUNT) {
      var i = Random.nextInt(FIELD_SIZE)
      var j = Random.nextInt(FIELD_SIZE)

      while (field(i)(j).isObst || field(i)(j).isStart || field(i)(j).isFinish) {
        i = Random.nextInt(FIELD_SIZE)
        j = Random.nextInt(FIELD_SIZE)
      }

      field(i)(j).char = cObst
    }
  }

  def run(): Unit = {
    var current: Point = start

    val tStart = System.nanoTime()
    while (current != finish) {
      current.calcNeighbours()
      field(current.y)(current.x).setDead()
//      show()

      current = current.chooseNext()
      field(current.y)(current.x).setCurrent()
//      show()
    }
    val tFinish = System.nanoTime() - tStart

    while (current != start) {
      field(current.y)(current.x).setResult()
      current = field(current.y)(current.x).from
    }

    field(start.y)(start.x).char = cStart
    field(finish.y)(finish.x).char = cFinish
    show()
    println()
  }

  def show(): Unit = {
    println()
    for (i <- 0 until FIELD_SIZE) {
      for (j <- 0 until FIELD_SIZE) {
        print(s"${field(i)(j).char.toString} | ")
      }

      println()

      for (j <- 0 until FIELD_SIZE) {
        print("- + ")
      }

      println()
    }
  }
}
