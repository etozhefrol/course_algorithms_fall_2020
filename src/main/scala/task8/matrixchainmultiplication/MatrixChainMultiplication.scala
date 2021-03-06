package task8.matrixchainmultiplication

import scala.util.Random

object Run extends App {
  import MatrixChainMultiplication._

//  val testP = Array(10, 35, 15, 5, 10, 20, 25)
    val testP = Array(10, 1000, 50, 5, 100, 50)

  val a1: Array[Array[Int]] = Array.fill(1000, 100000)(Random.nextInt())
  val a2: Array[Array[Int]] = Array.fill(100000, 500)(Random.nextInt())
  val a3: Array[Array[Int]] = Array.fill(500, 50)(Random.nextInt())
  val a4: Array[Array[Int]] = Array.fill(50, 10000)(Random.nextInt())
  val a5: Array[Array[Int]] = Array.fill(10000, 5000)(Random.nextInt())

  println("sizes: " + testP.mkString(","))


  val res = MatrixChainMultiplication.matrixChainOrder(testP)

  print("optimal parenthesization: ")
  MatrixChainMultiplication.printOptimalParens(res._2, 0, testP.length - 2)
  println()

  val start = System.nanoTime()

  p(p(p(p(a1, a2), a3), a4), a5)

  println("straight calculation:   " + (System.nanoTime() - start))

  val start2 = System.nanoTime()

  p(p(a1, p(a2, a3)), p(a4, a5))

  println("optimal multiplication: " + (System.nanoTime() - start2))

}

object MatrixChainMultiplication {
  def matrixChainOrder(p: Array[Int]): (Array[Array[Int]], Array[Array[Int]]) = {
    val n = p.length - 1      // 3
    val m: Array[Array[Int]] = Array.fill(n)(Array.fill(n)(0))
    val s: Array[Array[Int]] = Array.fill(n - 1)(Array.fill(n)(0))

    for {
      l <- 1 until n          // 1    2
      i <- 0 until n - l      // 0,1  0
    } {
      val j = i + l           // 1,2  2
      m(i)(j) = Int.MaxValue

      for (k <- i until j) {  // 0,1  0,1
        val q = m(i)(k) + m(k + 1)(j) + p(i) * p(k + 1) * p(j + 1)  // [(0,0),(1,1)  (0,0),(0,1)] + [(1,1),(2,2)  (1,2),(2,2)] + [(0,1,2),(1,2,3)  (0,1,3),(0,2,3)]

        if (q <= m(i)(j)) {
          m(i)(j) = q
          s(i)(j) = k
        }
      }
    }

    (m, s)
  }

  def printOptimalParens(s: Array[Array[Int]], i: Int, j: Int): Unit = {
    if (i == j) print("A" + i)
    else {
      print("(")
      printOptimalParens(s, i, s(i)(j))
      printOptimalParens(s, s(i)(j) + 1, j)
      print(")")
    }
  }

  def p(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    val res: Array[Array[Int]] = Array.ofDim(a.length, b(0).length)

    for {
      i <- res.indices
      j <- res(0).indices
      k <- b.indices
    } {
      res(i)(j) += a(i)(k) * b(k)(j)
    }

    res
  }
}
