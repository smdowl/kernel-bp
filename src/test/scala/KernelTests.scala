import breeze.linalg.{DenseMatrix, DenseVector}
import kernel.RBFKernel
import org.scalatest._

class KernelTests extends FunSuite with Matchers {

  test("RBF kernel") {
    val kernel = new RBFKernel

    val p1 = DenseVector(1.0, 1.0)
    kernel(p1, p1, 0.5) shouldBe DenseMatrix((1.0, 1.0), (1.0, 1.0))
  }
}
