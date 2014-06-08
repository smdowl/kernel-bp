package parser

trait Trainable {
  def train(histories: Seq[ParseHistory])
}
