package kernel

import breeze.linalg.{DenseVector, DenseMatrix, all}

package object linalg {
  def nearlyEqual(m1: DenseMatrix[Double], m2: DenseMatrix[Double], bound: Double=1.0e-5): Boolean = {
    all(m1 :< m2 + bound) && all(m1 :> m2 - bound)
  }

  def vec2mat(vec: DenseVector[Double]): DenseMatrix[Double] = {
    if (vec == null)
      null
    else
      DenseMatrix(vec.toArray)
  }

  def meshgrid(x1: DenseVector[Double], x2: DenseVector[Double]): DenseMatrix[Double] = {
    val x1Mesh = DenseMatrix.zeros[Double](x2.length, x1.length)
    for (i <- 0 until x2.length) {
      x1Mesh(i, ::) := x1.t
    }
    val x2Mesh = DenseMatrix.zeros[Double](x2.length, x1.length)
    for (i <- 0 until x1.length) {
      x2Mesh(::, i) := x2
    }

    DenseMatrix(x2Mesh.toDenseVector.toArray, x1Mesh.toDenseVector.toArray).t
  }
}
