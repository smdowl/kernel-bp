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

//    plotData(sampleArr)

    // Other params
    val observedList = Seq(4)
    val observations = Seq(0)

    // Parzen window parameter at root
    val sigRoot = 0.1

    val cache = buildCache(model, sampleArr)

    // INFERENCE START

    val betaArr = Array.ofDim[Vector[Double]](model.numNodes - observedList.length)

    for ((leafId, observation) <- observedList zip observations) {
      val parentId = model.getParents(leafId)(0)

      val Kt = cache.kArr(leafId)(leafId)
      val Ks = cache.kArr(leafId)(parentId)
      val I = DenseMatrix.eye[Double](Kt.rows)

      val kt = rbfDot(cache.leafArr(leafId), DenseVector(observation), model.msgParam.sig)

      // Have to split because type seems not to be infered otherwise
      val left: DenseMatrix[Double] = Kt :+ I * model.msgParam.lambda
      betaArr(leafId) = (left * (Ks + I * model.msgParam.lambda)) \ kt
    }
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

  def buildCache(model: Model, sampleArr: DenseMatrix[Double]): Cache = {

    val numNodes = model.numNodes
    val sig = model.msgParam.sig

    val kArr = Array.ofDim[DenseMatrix[Double]](numNodes, numNodes)
    val leafArr = Array.ofDim[Vector[Double]](numNodes)

    for (nodeInd <- 0 until numNodes) {
      val children = model.getChildren(nodeInd)
      for (childInd <- children)
        kArr(nodeInd)(childInd) = rbfDot(sampleArr(::, nodeInd), sampleArr(::, nodeInd), sig)

      if (children.length == 0) {
        kArr(nodeInd)(nodeInd) = rbfDot(sampleArr(::, nodeInd), sampleArr(::, nodeInd), sig)
        leafArr(nodeInd) = sampleArr(::, nodeInd)
      }

      for (parentInd <- model.getParents(nodeInd))
        kArr(nodeInd)(parentInd) = rbfDot(sampleArr(::, parentInd), sampleArr(::, parentInd), sig)
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
}