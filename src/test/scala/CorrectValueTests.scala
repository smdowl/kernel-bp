import app.Constants
import breeze.linalg.{DenseMatrix, DenseVector}
import io.MatrixReader
import kernel._
import kernel.linalg.{vec2mat, nearlyEqual}
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}
import scala.util.Random

class CorrectValueTests extends Test {
  var model: DemoModel = _
  var passer: MessagePasser = _
  var betaArr: Array[DenseMatrix[Double]] = _

  before {
    val numSamples = 20
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
      allNearlyEqualArrays(row1, row2)
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

  test("Any betas match") {
    val trueArr = model.loadCorrect()
    anyNearlyEqualArrays(trueArr, betaArr)
  }

  test("All betas match") {
    val trueArr = model.loadCorrect()
    allNearlyEqualArrays(trueArr, betaArr)
  }

  def allNearlyEqualArrays(a1: Array[DenseMatrix[Double]], a2: Array[DenseMatrix[Double]]) = {
    nearlyEqualArrays(a1, a2).forall(_ == true) shouldBe true
  }

  def anyNearlyEqualArrays(a1: Array[DenseMatrix[Double]], a2: Array[DenseMatrix[Double]]) = {
    nearlyEqualArrays(a1, a2).exists(_ == true) shouldBe true
  }

  def nearlyEqualArrays(a1: Array[DenseMatrix[Double]], a2: Array[DenseMatrix[Double]]): Array[Boolean] = {
    a1.length shouldEqual a2.length

    val matches = (a1 zip a2).map {
      case (m1: DenseMatrix[Double], m2: DenseMatrix[Double]) =>
          nearlyEqual(m1, m2, 1.0e-2)
      case t: (Any, Any) => t._1 == t._2
    }

    val (n, m) = getRandomElementInMatrix(a1)

    for (i <- 0 until a1.length) {
      println(matches(i), nullOrElementAtIndex(a1(i), n, m), nullOrElementAtIndex(a2(i), n, m))
    }

    matches
  }

  def getRandomElementInMatrix(arr: Array[DenseMatrix[Double]]) = {
    val (rows, cols) = sizeOfMatricesInArray(arr)
    (Random.nextInt(rows), Random.nextInt(cols))
  }

  def sizeOfMatricesInArray(arr: Array[DenseMatrix[Double]]) = {
    var i = 0
    while (i < arr.length && arr(i) == null)
      i += 1

    if (i < arr.length)
      (arr(i).rows, arr(i).cols)
    else
      (0, 0)
  }

  def nullOrElementAtIndex(m: DenseMatrix[Double], i: Int, j: Int) = {
    if (m != null)
      m(i,j)
    else
      null
  }
}
