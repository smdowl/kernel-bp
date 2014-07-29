package kernel.models.edge

import breeze.linalg.{DenseMatrix}

trait EdgeModel {
  def edges: Map[String, Edge]
  def testObservations: Array[Map[Int, DenseMatrix[Double]]]
  def testLabels: Array[Map[Int, DenseMatrix[Double]]]
  def keyArray: Array[String]
}
