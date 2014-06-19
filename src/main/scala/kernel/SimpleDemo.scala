package kernel

import breeze.linalg._
import breeze.plot._

case class MessageParam(lambda: Double, sig: Double)

object SimpleDemo extends App {

  val Vector = DenseVector
  val Matrix = DenseMatrix

  val A = Matrix(
    (0,1,1,0,0),
    (0,0,0,1,1),
    (0,0,0,0,0),
    (0,0,0,0,0),
    (0,0,0,0,0) )

  val numNodes = A.rows
  val rootNode = 1

  val rootMeans = DenseVector(0,2)
  val rootStd = 0.2
  val p1Root = 0.5

  val middleStd = 0.2

  val p1Leaf = 0.5
  val leafStd = 0.2

  val nodeOrder = 0 until numNodes

  val sigRoot = 0.1

  val n = 2000

  val observedList = Vector(4)
  val observations = Vector(0)

  val msgParam = MessageParam(0.1, 0.3)

  val sampleArr = generateData()

  val f = Figure()
  for (i <- 0 until numNodes) {
    val p = f.subplot(3, 2, i)
    p += hist(sampleArr(::, i))
    p.title = s"Node $i"
  }

//  println(sampleArr)

  def generateData(): DenseMatrix[Double] = {
    val sampArr = DenseMatrix.zeros[Double](n, numNodes)

    for (sampleInd <- 0 until n) {
      for (nodeInd <- 0 until numNodes) {

        val whichNode = nodeOrder(nodeInd)
        val parentInd = getParents(A, whichNode)

        if (parentInd.length >= 1) { // Middle or leaf

          if (getChildren(A, whichNode).length >= 1) {
            val mu = sampArr(sampleInd, parentInd(0)) // Only one parent in this graph
            sampArr(sampleInd, whichNode) = mu + middleStd * randn()
          } else {
            val mu = sampArr(sampleInd, parentInd(0))
            val c = rand()
            if (c <= p1Leaf)
              sampArr(sampleInd, whichNode) = mu + leafStd * randn()
            else
              sampArr(sampleInd, whichNode) = 0 + leafStd * randn()
          }
        } else { // ROOT node
          val c = rand()
          if (c <= p1Root)
            sampArr(sampleInd, whichNode) = rootMeans(0) + rootStd * randn()
          else
            sampArr(sampleInd, whichNode) = rootMeans(1) + rootStd * randn()
        }
      }
    }

    sampArr
  }

  def getParents(A: DenseMatrix[Int], idx: Int): Seq[Int] = A(::, idx).findAll(_ > 0)
  def getChildren(A: DenseMatrix[Int], idx: Int): Seq[Int] = A(idx, ::).t.findAll(_ > 0)
}
