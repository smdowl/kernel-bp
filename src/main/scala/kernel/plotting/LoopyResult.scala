package kernel.plotting

import breeze.linalg.DenseMatrix
import kernel.kernels.Kernel
import kernel.models.Model

case class LoopyResult(model: Model,
                       kernel: Kernel,
                       sampleArr: Array[DenseMatrix[Double]],
                       observations: Map[Int, DenseMatrix[Double]],
                       betaArr: Array[Array[DenseMatrix[Double]]],
                       sigRoot: Double,
                       support: DenseMatrix[Double])
