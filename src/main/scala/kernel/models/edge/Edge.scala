package kernel.models.edge

import breeze.linalg.CSCMatrix

case class Edge(startData: CSCMatrix[Double], endData: CSCMatrix[Double])
