package task8

object Task extends App {
  val testP = Array(30, 35, 15, 5, 10, 20, 25)
//  val testP = Array(10, 100, 5, 50)

  val res = MatrixChainMultiplication.matrixChainOrder(testP)

  MatrixChainMultiplication.printOptimalParens(res._2, 0, 5)
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
}