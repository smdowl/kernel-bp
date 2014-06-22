package kernel

import breeze.linalg._
import breeze.numerics._

trait Kernel {
  def apply(p1: DenseVector[Double], p2: DenseVector[Double], deg: Double = 1.0): DenseMatrix[Double]
}

class RBFKernel extends Kernel {
  override def apply(p1Vec: DenseVector[Double], p2Vec: DenseVector[Double], deg: Double = 1.0): DenseMatrix[Double] = {

    val n1 = p1Vec.length
    val n2 = p2Vec.length

    val p1 = DenseMatrix(p1Vec.toArray)
    val p2 = DenseMatrix(p2Vec.toArray)

    val G: DenseMatrix[Double] = sum(p1 :* p1, Axis._0).asInstanceOf[DenseMatrix[Double]].t
    val H: DenseMatrix[Double] = sum(p2 :* p2, Axis._0).asInstanceOf[DenseMatrix[Double]].t

    val Q = repmat(G, 1, n2)
    val R = repmat(H.t, n1, 1)

    val T: DenseMatrix[Double] = Q + R
    val r: DenseMatrix[Double] = (p1.t * p2).asInstanceOf[DenseMatrix[Double]]

    val out: DenseMatrix[Double] = -(T - r * 2.0).asInstanceOf[DenseMatrix[Double]] / (2 * Math.pow(deg, 2))
    exp(out)
  }

  private def repmat(A: DenseMatrix[Double], m: Int, n: Int): DenseMatrix[Double] = {
    assert(m > 0)
    assert(n > 0)

    val row: DenseMatrix[Double] = DenseMatrix.horzcat((for (i <- 0 until n) yield A):_*)

    DenseMatrix.vertcat((for (j <- 0 until m) yield row):_*)
  }
}