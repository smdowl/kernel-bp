import breeze.linalg.DenseMatrix
import kernel.DemoModel
import kernel.models.{DemoModel, Model}
import org.scalatest.{BeforeAndAfter, Matchers, FunSuite}

class ModelTests extends Test {
  var model: Model = _

  before {
    model = new DemoModel(10)
  }

  test("Pruning") {
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

  test("Getting parents") {
    model.getParents(0) shouldBe Seq[Int]()
    model.getParents(1) shouldBe Seq[Int](0)
    model.getParents(2) shouldBe Seq[Int](0)
    model.getParents(3) shouldBe Seq[Int](1)
    model.getParents(4) shouldBe Seq[Int](1)
  }
}
