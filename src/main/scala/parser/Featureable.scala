package parser

trait Featureable {
  def featuresWithKey(key: String): Map[String, Int]
}
