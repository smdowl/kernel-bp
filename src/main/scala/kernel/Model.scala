package kernel

import breeze.linalg._

abstract class Model(val n: Int) {
  val A: DenseMatrix[Int]
  val msgParam: MessageParam

  def numNodes: Int = A.rows
  def getPrunedTree(observedList: Iterable[Int]): DenseMatrix[Int]

  def getParents(nodeId: Int): Seq[Int] = A(::, nodeId).findAll(_ > 0)
  def getChildren(nodeId: Int): Seq[Int] = try {A(nodeId, ::).t.findAll(_ > 0)} catch {case _ => Seq[Int]()}

  def generateData(): DenseMatrix[Double]
}
