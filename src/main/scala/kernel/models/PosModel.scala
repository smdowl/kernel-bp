package kernel.models

abstract class PosModel(n: Int, length: Int) extends ChainModel(n, length) with ParsedModel {
  def labelKeys: Set[String]
  def getIndexForKey(key: String): Int
}
