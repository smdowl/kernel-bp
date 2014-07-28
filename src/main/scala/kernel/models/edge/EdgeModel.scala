package kernel.models.edge

import breeze.linalg.DenseVector

abstract class EdgeModel {
  def edges: Map[String, Edge]
  def testData: Array[Array[DenseVector[Double]]]
  def keyArray: Array[String]
}
