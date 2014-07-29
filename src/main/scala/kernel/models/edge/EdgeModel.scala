package kernel.models.edge

import breeze.linalg.{DenseMatrix}

trait EdgeModel {
  def edges: Map[String, Edge]
  def testObservations: Array[Map[Int, DenseMatrix[Double]]]
  def testLabels: Array[Map[Int, DenseMatrix[Double]]]
  def keyArray: Array[String]

  def keyIndex: Map[String, Int] = {
    Map[String, Int]() ++ (0 until keyArray.length).map(i => {
      this.keyArray(i) -> i
    })
  }
}
