import app.Constants
import breeze.linalg.DenseMatrix
import io.MatrixReader
import org.scalatest.{BeforeAndAfter, Matchers, FunSuite}
import kernel.linalg.nearlyEqual

class MatrixOperationTests extends FunSuite with Matchers with BeforeAndAfter {
  test("External messages") {
    val lambda = 0.1

    val Kt = loadExternalMatrix("Kt")
    val Ks = loadExternalMatrix("Ks")
    val I = loadExternalMatrix("I")

    val kt = loadExternalMatrix("small-kt")

    val trueLeft = loadExternalMatrix("left")
    val trueRight = loadExternalMatrix("right")
    val trueLr = loadExternalMatrix("lr")
    val trueSol = loadExternalMatrix("sol")

    val left: DenseMatrix[Double] = Kt :+ I * lambda
    val right: DenseMatrix[Double] = Ks + I * lambda
    val lr = left.t * right
    val sol = lr \ kt

    nearlyEqual(trueLeft, left, 1e-5) shouldBe true
    nearlyEqual(trueRight, right, 1e-5) shouldBe true
    nearlyEqual(trueLr, lr, 1e-5) shouldBe true
    nearlyEqual(trueSol, sol, 1e-5) shouldBe true
  }

  def loadExternalMatrix(mat: String) = MatrixReader.loadMatrixFromFile(Constants.INTER_MATRICES_DIR + "obs/" + mat)

  test("Internal messages") {
    val lambda = 0.1
    val Ks = loadExternalMatrix("Ks")

    val nts = Ks.rows
    val ones = DenseMatrix.ones[Double](nts, 1)

    val Karr = loadExternalMatrix("Karr")
    val betaArr = loadExternalMatrix("betaArr")

    val trueKtu_beta = loadExternalMatrix("Ktu_beta")
    val Ktu_beta = ones :* (Karr * betaArr)

//    val trueLeft = loadExternalMatrix("left")
//    val trueRight = loadExternalMatrix("right")
//    val trueLr = loadExternalMatrix("lr")
    val trueSol = loadExternalMatrix("sol")
    val sol = (Ks + DenseMatrix.eye[Double](nts) * lambda) \ Ktu_beta

    nearlyEqual(trueKtu_beta, Ktu_beta, 1e-5) shouldBe true
    nearlyEqual(trueSol, sol, 1e-5) shouldBe true

  }

  def loadInternalMatrix(mat: String) = MatrixReader.loadMatrixFromFile(Constants.INTER_MATRICES_DIR + "int/" + mat)
}
