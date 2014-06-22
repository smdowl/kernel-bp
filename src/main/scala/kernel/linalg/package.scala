package kernel

import breeze.linalg.{DenseVector, DenseMatrix, all}

package object linalg {
  def nearlyEqual(m1: DenseMatrix[Double], m2: DenseMatrix[Double]): Boolean = {
    val bound = 1.0e-5
    all(m1 :< m2 + bound) && all(m1 :> m2 - bound)
  }

  def vec2mat(vec: DenseVector[Double]): DenseMatrix[Double] = {
    if (vec == null)
      null
    else
      DenseMatrix(vec.toArray)
  }
}
