package kernel

import breeze.linalg._
import breeze.plot._

case class MessageParam(lambda: Double, sig: Double)
case class Cache(kArr: Array[Array[DenseMatrix[Double]]], leafArr: Array[Vector[Double]])

object SimpleDemo extends App {

  val A = DenseMatrix(
    (0,1,1,0,0),
    (0,0,0,1,1),
    (0,0,0,0,0),
    (0,0,0,0,0),
    (0,0,0,0,0) )

  val rootNode = 0
  val numNodes = A.rows
  val numSamples = 200

  val sampleArr = generateData(A, numSamples)

//  plotData(sampleArr)

  // Other params
  val observedList = DenseVector(4)
  val observations = DenseVector(0)

  val msgParam = MessageParam(0.1, 0.3)

  // Parzen window parameter at root
  val sigRoot = 0.1

  val cache = buildCache(A, sampleArr, msgParam)
  println()

  def generateData(A: DenseMatrix[Int], n: Int): DenseMatrix[Double] = {

    // Model parameters
    val rootMeans = DenseVector(0,2)
    val rootStd = 0.2
    val p1Root = 0.5

    val middleStd = 0.2

    val leafStd = 0.2
    val p1Leaf = 0.5

    val numNodes = A.rows

    val sampleOrder = 0 until numNodes

    // Sampling step
    val outputArray = DenseMatrix.zeros[Double](n, numNodes)

    for (sampleInd <- 0 until n) {
      for (nodeInd <- 0 until numNodes) {

        val whichNode = sampleOrder(nodeInd)
        val parentInd = getParents(A, whichNode)

        if (parentInd.length >= 1) { // Middle or leaf

          if (getChildren(A, whichNode).length >= 1) {
            val mu = outputArray(sampleInd, parentInd(0)) // Only one parent in this graph
            outputArray(sampleInd, whichNode) = mu + middleStd * randn()
          } else {
            val mu = outputArray(sampleInd, parentInd(0))
            val c = rand()
            if (c <= p1Leaf)
              outputArray(sampleInd, whichNode) = mu + leafStd * randn()
            else
              outputArray(sampleInd, whichNode) = 0 + leafStd * randn()
          }
        } else { // ROOT node
        val c = rand()
          if (c <= p1Root)
            outputArray(sampleInd, whichNode) = rootMeans(0) + rootStd * randn()
          else
            outputArray(sampleInd, whichNode) = rootMeans(1) + rootStd * randn()
        }
      }
    }

    outputArray
  }

  def getParents(A: DenseMatrix[Int], idx: Int): Seq[Int] = A(::, idx).findAll(_ > 0)
  def getChildren(A: DenseMatrix[Int], idx: Int): Seq[Int] = try {A(idx, ::).t.findAll(_ > 0)} catch {case _ => Seq[Int]()}

  def plotData(data: DenseMatrix[Double]) = {
    val numNodes = data.cols

    val f = Figure()
    for (i <- 0 until numNodes) {
      val p = f.subplot(3, 2, i)
      p += hist(data(::, i))
      p.title = s"Node $i"
    }
  }

  def buildCache(A: DenseMatrix[Int], sampleArr: DenseMatrix[Double], msgParam: MessageParam): Cache = {
    val numNodes = A.rows

    val kArr = Array.ofDim[DenseMatrix[Double]](numNodes, numNodes)
    val leafArr = Array.ofDim[Vector[Double]](numNodes)

    for (nodeInd <- 0 until numNodes) {
      val children = getChildren(A, nodeInd)
      for (childInd <- children)
        kArr(nodeInd)(childInd) = rbfDot(sampleArr(::, nodeInd), sampleArr(::, nodeInd), msgParam.sig)

      if (children.length == 0) {
        kArr(nodeInd)(nodeInd) = rbfDot(sampleArr(::, nodeInd), sampleArr(::, nodeInd), msgParam.sig)
        leafArr(nodeInd) = sampleArr(::, nodeInd)
      }

      for (parentInd <- getParents(A, nodeInd))
        kArr(nodeInd)(parentInd) = rbfDot(sampleArr(::, parentInd), sampleArr(::, parentInd), msgParam.sig)
    }

    Cache(kArr, leafArr)
  }

  def rbfDot(p1: Vector[Double], p2: Vector[Double], deg: Double = 1.0): DenseMatrix[Double] = {
    // TODO: Currently slow version, but fast version is in MATLAB code
    val out = DenseMatrix.zeros[Double](p1.length, p2.length)

    for (i <- 0 until p1.length)
      for (j <- 0 until p2.length)
        out(i, j) = Math.exp(- Math.pow(p1(i) - p2(j), 2) / (2 * Math.pow(deg, 2)))

    out
  }

  def other = {
    //  val betaArr = Array.ofDim[Vector[Double]](numNodes - observedList.length)

    //  for ((leafInd, observation) <- observedList zip observations) {
    //    val parentInd = getParents(A, leafInd)(0)
    //
    //    val Kt = cache.kArr(leafInd)(leafInd)
    //    val Ks = cache.kArr(leafInd)(parentInd)
    //    val I = DenseMatrix.eye[Double](Kt.rows)
    //
    //    val kt = rbfDot(cache.leafArr(leafInd), DenseVector(observation), msgParam.sig)
    //    println()
    ////    val obj = (Kt :+ I * msgParam.lambda)
    ////    obj
    ////    betaArr(leafInd) = (obj * (Ks + I * msgParam.lambda)) \ kt
    //  }
  }
}
