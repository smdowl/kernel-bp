package kernel

import breeze.linalg._

abstract class Model(val n: Int) {
  var A: DenseMatrix[Int]
  val msgParam: MessageParam
  val rootNode: Int

  def numNodes: Int = A.rows
  def getPrunedTree(observedNodes: Set[Int]): (DenseMatrix[Int], Set[Int])

  def getParents(nodeId: Int, A: DenseMatrix[Int] = this.A): Seq[Int] = A(::, nodeId).findAll(_ > 0)
  def getChildren(nodeId: Int, A: DenseMatrix[Int] = this.A): Seq[Int] = try {A(nodeId, ::).t.findAll(_ > 0)} catch {case _ => Seq[Int]()}

  def generateData(): DenseMatrix[Double]

  def loadCorrect(): Array[DenseMatrix[Double]]
}
