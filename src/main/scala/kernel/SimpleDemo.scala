package kernel

import breeze.linalg._
import breeze.plot._

case class MessageParam(lambda: Double, sig: Double)
case class Cache(kArr: Array[Array[DenseMatrix[Double]]], leafArr: Array[Vector[Double]])

object SimpleDemo {

  def main(args: Array[String]) = {

    val numSamples = 200
    val model: Model = new DemoModel(numSamples)

    val sampleArr = model.generateData()

    plotData(sampleArr)

    // Other params
    val observedList = DenseVector(4)
    val observations = DenseVector(0)

    val msgParam = MessageParam(0.1, 0.3)

    // Parzen window parameter at root
    val sigRoot = 0.1

//    val cache = buildCache(A, sampleArr, msgParam)
    println()
  }

  def plotData(data: DenseMatrix[Double]) = {
    val numNodes = data.cols

    val f = Figure()
    for (i <- 0 until numNodes) {
      val p = f.subplot(3, 2, i)
      p += hist(data(::, i))
      p.title = s"Node $i"
    }
  }

  def buildCache(model: Model, sampleArr: DenseMatrix[Double], msgParam: MessageParam): Cache = {
    val kArr = Array.ofDim[DenseMatrix[Double]](model.numNodes, model.numNodes)
    val leafArr = Array.ofDim[Vector[Double]](model.numNodes)

    for (nodeInd <- 0 until model.numNodes) {
      val children = model.getChildren(nodeInd)
      for (childInd <- children)
        kArr(nodeInd)(childInd) = rbfDot(sampleArr(::, nodeInd), sampleArr(::, nodeInd), msgParam.sig)

      if (children.length == 0) {
        kArr(nodeInd)(nodeInd) = rbfDot(sampleArr(::, nodeInd), sampleArr(::, nodeInd), msgParam.sig)
        leafArr(nodeInd) = sampleArr(::, nodeInd)
      }

      for (parentInd <- model.getParents(nodeInd))
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

  def other() = {
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
