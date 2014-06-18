package dependencies.parser

trait Trainable {
  def train(histories: Seq[ParseHistory])
}
