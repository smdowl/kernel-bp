import app.Constants
import breeze.linalg.{max, DenseMatrix}
import breeze.numerics.abs
import io.MatrixReader
import kernel.caches.{LoopyCache, Cache}
import kernel.kernels.RBFKernel
import kernel.linalg.nearlyEqual
import kernel.models.{LoopyDemoModel, DemoModel}
import kernel.propagation.{LoopyMessagePasser, TreeMessagePasser}

import scala.util.Random

class LoopyCorrectValueTests extends Test {
  var model: LoopyDemoModel = _
  var passer: LoopyMessagePasser = _
  var betaArr: Array[Array[DenseMatrix[Double]]] = _
  var observations: Map[Int, DenseMatrix[Double]] = _

  before {
    val numSamples = 2
    model = new LoopyDemoModel(numSamples, Constants.LOOPY_SAMPLE_DATA)

    val sampleArr = model.generateData()
    observations = Map(3 -> DenseMatrix(0.0))

    val kernel = new RBFKernel()
    passer = new LoopyMessagePasser(model, kernel, sampleArr, observations.keySet)

    betaArr = passer.passMessages(observations)
  }

  test("Cache test") {
    val realCache = loadTestCache()
    testCacheSimilarity(passer.getCache, realCache)
  }

  def loadTestCache(): LoopyCache = {
    val kArr = loadKArr()

    new LoopyCache(kArr)
  }

  def loadKArr() = {
    var rows = Seq[DenseMatrix[Double]]()
    for (i <- 1 to model.numNodes)
      rows :+= MatrixReader.loadMatrixFromFile(Constants.LOOPY_CORRECT_DIR + s"Karr$i")
    rows.toArray
  }


  def testCacheSimilarity(c1: LoopyCache, c2: LoopyCache) = {
    allNearlyEqualArrays(c1.kArr, c2.kArr)
  }


  test("Any betas match") {
    val trueArr = loadCorrect()
    for (i <- 0 until model.numNodes)
      if (trueArr(i) != null)
        anyNearlyEqualArrays(trueArr(i), betaArr(i))
  }

  test("All betas match") {
    val trueArr = loadCorrect(shouldNorm = false)
    for (i <- 0 until model.numNodes)
      if (trueArr(i) != null)
        allNearlyEqualArrays(trueArr(i), betaArr(i))
  }

  test("Observed messages") {
    val trueArr = loadCorrect(shouldNorm = true)
    for (obsId <- observations.keySet) {
      val neighbours = model.getNeighbours(obsId)

      for (neighbour <- neighbours) {
        val v1 = betaArr(obsId)(neighbour)
        val v2 = trueArr(obsId)(neighbour)
        val matches = nearlyEqual(v1, v2)
        println(matches, v1, v2)

        matches shouldBe true
      }
    }
  }

  def loadCorrect(shouldNorm: Boolean = false): Array[Array[DenseMatrix[Double]]] = {
    val output = Array.ofDim[Array[DenseMatrix[Double]]](model.numNodes)

    for (i <- 1 to model.numNodes) {
      output(i-1) = Array.ofDim[DenseMatrix[Double]](model.numNodes)
      for (j <- 1 to model.numNodes) {
        val filepath = Constants.LOOPY_CORRECT_DIR + s"betaArr$i$j"
        output(i-1)(j-1) = try {
          val matrix = MatrixReader.loadMatrixFromFile(filepath)

          if (shouldNorm)
            matrix / abs(max(matrix))
          else
            matrix
        } catch {case _: Throwable => null}
      }
    }

    output.toArray
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
