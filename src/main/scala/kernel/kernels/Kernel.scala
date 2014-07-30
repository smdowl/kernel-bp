package kernel.kernels

import breeze.linalg._
import breeze.numerics._

trait Kernel {
  def apply(p1: Matrix[Double], p2: Matrix[Double], deg: Double = 1.0): DenseMatrix[Double]
}

