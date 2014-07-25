import breeze.linalg.DenseMatrix
import kernel.caches.EdgeBasedCache
import kernel.models.toys.DeterministicHMMModel
import kernel.kernels.LinearKernel

class EdgeCacheTests extends Test {

  test("Single data test") {
    val length = 3

    val model = new DeterministicHMMModel(1, length)
    val sampleArr = genSingleDataPoint(model.numNodes)
    val kernel = new LinearKernel()

    val cache = EdgeBasedCache.buildCache(sampleArr, kernel, model)

    println(cache)
  }

  private def genSingleDataPoint(numNodes: Int) = {
    val sampleArr = Array.ofDim[DenseMatrix[Double]](numNodes)
    for (i <- 0 until numNodes)
      sampleArr(i) = DenseMatrix.ones[Double](1,1) * i.toDouble

    sampleArr
  }

}
