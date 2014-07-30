package kernel.incomplete

import breeze.linalg._

import scala.collection.mutable.ListBuffer

object IncompleteCholesky {

  case class Result(K_chol: DenseMatrix[Double], I: Seq[Int], R: DenseMatrix[Double], W: DenseMatrix[Double], acc: Double)

  def incompleteCholesky[T](X: Seq[T], kernel: (T, T) => Double, eta: Double, power: Double = 1.0, blockSize: Int = 1) = {
    //taken from https://github.com/karlnapf/graphlab/blob/master/apps/kernelbp/python_precompute_kernel_matrices/src/incomplete_cholesky.py

    val m = X.size

    //current list of evaluated kernel values
    var K = DenseMatrix.zeros[Double](m, blockSize)

    //current list of basis indices
    var I = new ListBuffer[Int]

    //??
    val nu = new ListBuffer[Double]

    //current low rank basis
    var R = DenseMatrix.zeros[Double](m, blockSize)

    //current index of basis
    var j = 0

    //diagonal (assumed to be one)
    //todo: shouldn't this be gram matrix diagonal? Only one if kernel (e.g. dot product) is normalized
    val d = DenseVector.ones[Double](m)

    //current error
    var a: Double = max(d)
    I += argmax(d)

    while (a > eta) {
      assert(j < m, "something's fishy")
      nu += math.sqrt(a)
      val current = X(I(j))

      //calculate next column of the kernel matrix
      if (power >= 1) {
        for (i <- 0 until m) K(i, j) = math.pow(kernel(current, X(i)), power)
      } else {
        for (i <- 0 until m) K(i, j) = 0.0
      }

      //product of current R and (transposed) row vector of R corresponding to chosen pivot.
      //each row vector of R corresponds to a linear combination of pivots 0 .. j
      val R_dot_j: DenseVector[Double] =
        if (j == 0) DenseVector.zeros[Double](m)
        else if (j == 1) R(::, 0) * R(I(j), 0)
        else R(0 until m, 0 until j) * R(I(j), 0 until j).t

      //The new current column vector
      R(::, j) := (K(::, j) - R_dot_j) / nu(j)

      //see how much we have covered each data points
      d := d - R(::, j).t * R(::, j)

      //how far is the least captured data point away?
      a = max(d)

      //what is the least captured data point?
      I += argmax(d)

      //next column
      j += 1

      //expand
      if (j >= K.cols) {
        K = DenseMatrix.horzcat(K, DenseMatrix.zeros[Double](m, blockSize))
        R = DenseMatrix.horzcat(R, DenseMatrix.zeros[Double](m, blockSize))
      }


    }

    //Remove unused columns (possible when blockSize > 1)
    K = K(0 until m, 0 until j)
    R = R(0 until m, 0 until j)

    //don't need the last pivot index
    I = I.dropRight(1)

    //"from low rank to full rank"
    //W=solve(R[:,I], R)
    val Rsquare = R(I, ::).toDenseMatrix
    val Rinv = inv(Rsquare) //is there a better way to solve for many b's?
    val W = R * Rinv //multiplying W with Rsquare gives the complete R

    //low rank K
    val Kchol = K(I, ::).toDenseMatrix


    Result(Kchol, I, R, W, a)
  }
}