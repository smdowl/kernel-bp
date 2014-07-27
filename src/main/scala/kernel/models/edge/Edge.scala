package kernel.models.edge

import breeze.linalg.DenseMatrix

case class Edge(startData: DenseMatrix[Double], endData: DenseMatrix[Double])
