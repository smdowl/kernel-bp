import app.Constants
import breeze.linalg.{DenseMatrix, DenseVector}
import io.MatrixReader
import kernel._
import kernel.linalg.{vec2mat, nearlyEqual}
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
    val realCache = loadTestCache()
    testCacheSimilarity(passer.getCache, realCache)
  }

  def testCacheSimilarity(c1: Cache, c2: Cache) = {
    nearlyEqualArrays(c1.leafArr.map(vec2mat), c2.leafArr.map(vec2mat))

    for ((row1, row2) <- c1.kArr zip c2.kArr)
      nearlyEqualArrays(row2, row2)
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
    nearlyEqualArrays(trueArr, betaArr)
  }

  def nearlyEqualArrays(a1: Array[DenseMatrix[Double]], a2: Array[DenseMatrix[Double]]) = {
    a1.length shouldEqual a2.length

    for ((m1, m2) <- a1 zip a2) {
      if (m1 == null || m2 == null)
        m1 shouldEqual m2
      else
        nearlyEqual(m1, m2) shouldBe true
    }
  }
}
