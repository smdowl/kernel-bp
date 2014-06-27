package kernel.kernels

import breeze.linalg._
import breeze.numerics._

trait Kernel {
  def apply(p1: DenseMatrix[Double], p2: DenseMatrix[Double], deg: Double = 1.0): DenseMatrix[Double]
}

