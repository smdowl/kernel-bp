package kernel

import breeze.linalg._

trait Kernel {
  def apply(p1: DenseVector[Double], p2: DenseVector[Double], deg: Double = 1.0): DenseMatrix[Double]
}

object RBFKernel extends App {
  val kernel = new RBFKernel

  val p1 = DenseVector(1.0, 2.0, 3.0)
  kernel(p1, p1, 0.5)
}

class RBFKernel extends Kernel {
  override def apply(p1Vec: DenseVector[Double], p2Vec: DenseVector[Double], deg: Double = 1.0): DenseMatrix[Double] = {

    val n1 = p1Vec.length
    val n2 = p2Vec.length

    val p1 = DenseMatrix(p1Vec.toArray)
    val p2 = DenseMatrix(p2Vec.toArray)

    val p1Sq = p1 :* p1

    val G: DenseMatrix[Double] = sum(p1 :* p1, Axis._0).asInstanceOf[DenseVector[Double]].asDenseMatrix.t
    val H: DenseMatrix[Double] = sum(p2 :* p2, Axis._0).asInstanceOf[DenseVector[Double]].asDenseMatrix.t

    val Q = repmat(G, 1, n2)
    val R = repmat(H.t, n1, 1)

    val T: DenseMatrix[Double] = Q + R
    val r: DenseMatrix[Double] = p1 * p2.t

    T - r * 2.0
  }

  private def repmat(A: DenseMatrix[Double], m: Int, n: Int): DenseMatrix[Double] = {
    assert(m > 0)
    assert(n > 0)

    val row: DenseMatrix[Double] = DenseMatrix.horzcat((for (i <- 0 until n) yield A):_*)

    DenseMatrix.vertcat((for (j <- 0 until m) yield row):_*)
  }
}