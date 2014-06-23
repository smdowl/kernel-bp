import breeze.linalg.DenseMatrix
import kernel.DemoModel
import org.scalatest.{BeforeAndAfter, Matchers, FunSuite}

class PruneTests extends FunSuite with Matchers with BeforeAndAfter {
  test("Pruning") {
    val model = new DemoModel(10)

    val (prunedA, prunedNodes) = model.getPrunedTree(Set(3))
    val expectedA = DenseMatrix(
      (0,1,0,0,0),
      (0,0,0,1,0),
      (0,0,0,0,0),
      (0,0,0,0,0),
      (0,0,0,0,0)
    )

    val expectedPruned = Set(2, 4)

    prunedA shouldBe expectedA
    prunedNodes shouldBe expectedPruned

  }
}
