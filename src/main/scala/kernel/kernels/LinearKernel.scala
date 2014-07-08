package kernel.kernels

import breeze.linalg.DenseMatrix

class LinearKernel extends Kernel {
  override def apply(p1: DenseMatrix[Double], p2: DenseMatrix[Double], deg: Double = 1.0): DenseMatrix[Double] = {

    val n1 = p1.rows
    val n2 = p2.rows

    val out = DenseMatrix.zeros[Double](n1, n2)
    for (i <- 0 until n1)
      for (j <- 0 until n2)
        out(i, j) = p1(i, ::).t.dot(p2(j,::).t)

    out
  }
}