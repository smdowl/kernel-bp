package dependencies.features

class FeatureVector() {

  private var values: Map[String, Double] = Map()

  def dot(other: FeatureVector): Double = {
    var sum = 0.0

    keySet.intersect(other.keySet).foreach(key => {
      sum += this(key) * other(key)
    })

    sum
  }

  def distance(other: FeatureVector): Double = {
    var sum = 0.0
    (keys ++ other.keys).foreach(key => {
      sum += Math.pow(this(key) - other(key), 2)
    })
    Math.pow(sum, 0.5)
  }

  def add(key: String, value: Double = 1.0): Unit = {
    if (value == 0.0)
      values -= key

    values += (key -> value)
  }

  def merge(other: FeatureVector): FeatureVector = {
    val output = new FeatureVector()

    (keySet ++ other.keySet).foreach(key => {
      output.add(key) -> this(key) * other(key)
    })

    output
  }

  def keys = values.keys
  def keySet = values.keySet
  def apply(key: String) = values.getOrElse(key, 0.0)

  override def toString = values.toString()
}

object FeatureVector extends App {
  val v1 = new FeatureVector
  val v2 = new FeatureVector

  v1.add("a")
  assert(v1.distance(v2) == 1.0)

  v2.add("b")
  assert(v1.dot(v2) == 0.0)
  assert(v1.distance(v2) == Math.pow(2.0, 0.5))
}