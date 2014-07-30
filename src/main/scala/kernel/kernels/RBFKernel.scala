package kernel.kernels

import breeze.linalg._
import breeze.numerics.exp

class RBFKernel extends Kernel {
   override def apply(p1: Matrix[Double], p2: Matrix[Double], deg: Double = 1.0): DenseMatrix[Double] = {

     val m1: DenseMatrix[Double] = p1.toDenseMatrix
     val m2: DenseMatrix[Double] = p2.toDenseMatrix

     val n1 = m1.rows
     val n2 = m2.rows

     val G: DenseMatrix[Double] = sum(m1 :* m1, Axis._1).asInstanceOf[DenseVector[Double]].toDenseMatrix.t
     val H: DenseMatrix[Double] = sum(m2 :* m2, Axis._1).asInstanceOf[DenseVector[Double]].toDenseMatrix.t

     val Q = repmat(G, 1, n2)
     val R = repmat(H.t, n1, 1)

     val T: DenseMatrix[Double] = Q + R
     val r: DenseMatrix[Double] = m1 * m2.t

     val out: DenseMatrix[Double] = -(T - r * 2.0) / (2 * Math.pow(deg, 2))
     exp(out)
   }

   private def repmat(A: DenseMatrix[Double], m: Int, n: Int): DenseMatrix[Double] = {
     assert(m > 0)
     assert(n > 0)

     val row: DenseMatrix[Double] = DenseMatrix.horzcat((for (i <- 0 until n) yield A):_*)

     DenseMatrix.vertcat((for (j <- 0 until m) yield row):_*)
   }
 }
