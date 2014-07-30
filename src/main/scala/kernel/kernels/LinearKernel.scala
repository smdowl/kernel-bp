package kernel.kernels

import breeze.linalg.{DenseVector, Matrix, DenseMatrix}

class LinearKernel extends Kernel {
  override def apply(p1: Matrix[Double], p2: Matrix[Double], deg: Double = 1.0): DenseMatrix[Double] = {

    val m1: DenseMatrix[Double] = p1.toDenseMatrix
    val m2: DenseMatrix[Double] = p2.toDenseMatrix

    val n1 = m1.rows
    val n2 = m2.rows

    val out = DenseMatrix.zeros[Double](n1, n2)
    for (i <- 0 until n1)
      for (j <- 0 until n2)
        out(i, j) = m1(i, ::).t.dot(m2(j,::).t)

    out
  }
}