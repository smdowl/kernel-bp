import breeze.linalg
import kernel.Model
import org.scalatest.{FunSuite, Matchers}

class CorrectValueTests extends FunSuite with Matchers {

  test("Should pass") {
    1 shouldEqual 1
  }

  test("Beta Test") {

  }

  def testBetaSimilarity(model: Model, betaArr: Array[linalg.DenseMatrix[Double]]) = {
    val trueArr = model.loadCorrect()

    for ((trueBeta, calcBeta) <- trueArr zip betaArr) {
      if (trueBeta == null || calcBeta == null)
        assert(trueBeta == calcBeta)
      else
        assert(nearlyEqual(trueBeta, calcBeta))
    }
  }

  def nearlyEqual(m1: linalg.DenseMatrix[Double], m2: linalg.DenseMatrix[Double]): Boolean = {
    val bound = 1.0e-5
    linalg.all(m1 :< m2 + bound) && linalg.all(m1 :> m2 - bound)
  }
}
