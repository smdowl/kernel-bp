package kernel

import breeze.linalg.{DenseMatrix, Vector}

trait Kernel {
  def apply(p1: Vector[Double], p2: Vector[Double], deg: Double = 1.0): DenseMatrix[Double]
}

class RBFKernel extends Kernel {
  override def apply(p1: Vector[Double], p2: Vector[Double], deg: Double = 1.0): DenseMatrix[Double] = {
    // TODO: Currently slow version, but fast version is in MATLAB code
    val out = DenseMatrix.zeros[Double](p1.length, p2.length)

    for (i <- 0 until p1.length)
      for (j <- 0 until p2.length)
        out(i, j) = Math.exp(- Math.pow(p1(i) - p2(j), 2) / (2 * Math.pow(deg, 2)))

    out
  }
}