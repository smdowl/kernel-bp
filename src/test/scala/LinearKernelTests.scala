import breeze.linalg.{max, DenseMatrix}
import kernel.kernels.{LinearKernel, RBFKernel}

class LinearKernelTests extends Test {
  val kernel = new LinearKernel

  test("Simple test") {
    val p1 = DenseMatrix(1.0, 1.0)
    kernel(p1, p1) === DenseMatrix((1.0, 1.0), (1.0, 1.0)) shouldBe true
  }

  test("Different dims") {
    val p1 = DenseMatrix(2.0, 4.0)
    val p2 = DenseMatrix(4.0)

    kernel(p1, p2) === DenseMatrix(8.0, 16.0) shouldBe true
  }
}
