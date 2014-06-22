import app.Constants
import breeze.linalg
import breeze.linalg.{DenseMatrix, DenseVector}
import io.MatrixReader
import kernel._
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

class CorrectValueTests extends FunSuite with Matchers with BeforeAndAfter {
  var model: Model = _
  var passer: MessagePasser = _
  var betaArr: Array[DenseMatrix[Double]] = _

  before {
    val numSamples = 400
    model = new DemoModel(numSamples, Constants.SAMPLE_DATA)

    val sampleArr = model.generateData()
    val observations = Map(3 -> DenseVector(2.0))

    val kernel = new RBFKernel()
    passer = new MessagePasser(model, kernel)

    betaArr = passer.passMessages(sampleArr, observations)
  }

  test("Cache test") {
    testCacheSimilarity(passer.getCache, loadTestCache())
  }

  def testCacheSimilarity(c1: Cache, c2: Cache) = {

  }

  def loadTestCache(): Cache = {
    val leafArr = loadLeafArr()
    val kArr = loadKArr()

    new Cache(kArr, leafArr)
  }

  def loadLeafArr() = {
    var row = Seq[DenseVector[Double]]()
    for (i <- 1 to model.numNodes) {
      val newMatrix = MatrixReader.loadMatrixFromFile(Constants.CORRECT_DIR + s"leafArr$i")
      if (newMatrix != null)
        row :+= newMatrix.toDenseVector
      else
        row :+= null
    }
    row.toArray
  }

  def loadKArr() = {
    var rows = Seq[Array[DenseMatrix[Double]]]()
    for (i <- 1 to model.numNodes)
      rows :+= loadKArrRow(i)
    rows.toArray
  }

  def loadKArrRow(i: Int) = {
    var row = Seq[DenseMatrix[Double]]()
    for (j <- 1 to model.numNodes)
      row :+= MatrixReader.loadMatrixFromFile(Constants.CORRECT_DIR + s"Karr$i$j")
    row.toArray
  }

  test("Betas match") {
    testBetaSimilarity(model, betaArr)
  }

  def testBetaSimilarity(model: Model, betaArr: Array[DenseMatrix[Double]]) = {
    val trueArr = model.loadCorrect()

    trueArr.length shouldEqual betaArr.length

    for ((trueBeta, calcBeta) <- trueArr zip betaArr) {
      if (trueBeta == null || calcBeta == null)
        trueBeta shouldEqual calcBeta
      else
        nearlyEqual(trueBeta, calcBeta) shouldBe true
    }
  }

  def nearlyEqual(m1: DenseMatrix[Double], m2: DenseMatrix[Double]): Boolean = {
    val bound = 1.0e-5
    linalg.all(m1 :< m2 + bound) && linalg.all(m1 :> m2 - bound)
  }
}
