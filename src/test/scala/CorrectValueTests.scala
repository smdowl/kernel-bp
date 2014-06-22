import app.Constants
import breeze.linalg
import breeze.linalg.DenseVector
import kernel.{MessagePasser, RBFKernel, DemoModel, Model}
import org.scalatest.{FunSuite, Matchers}

class CorrectValueTests extends FunSuite with Matchers {

  test("Betas match") {
    val numSamples = 200
    val model: Model = new DemoModel(numSamples, Constants.SAMPLE_DATA)

    val sampleArr = model.generateData()
    val observations = Map(3 -> DenseVector(2.0))

    val kernel = new RBFKernel()
    val passer = new MessagePasser(model, kernel)

    val betaArr = passer.passMessages(sampleArr, observations)
    testBetaSimilarity(model, betaArr)
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
