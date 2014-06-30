import kernel.models.LoopyDemoModel

class LoopyModelTests extends Test {
  var model: LoopyDemoModel = _
  before {
     model = new LoopyDemoModel(10)
  }

  test("Getting neighbours") {
    model.getNeighbours(0) shouldBe Seq[Int](1,2)
    model.getNeighbours(1) shouldBe Seq[Int](0,2,3)
    model.getNeighbours(2) shouldBe Seq[Int](0,1,3,4)
    model.getNeighbours(3) shouldBe Seq[Int](1,2)
    model.getNeighbours(4) shouldBe Seq[Int](2)
  }
}
