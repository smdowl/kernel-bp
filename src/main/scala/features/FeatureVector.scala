package features

class FeatureVector() {

  private var values: Map[String, Double] = Map()

  def dot(other: FeatureVector) = {
    val output = new FeatureVector()

    keySet.intersect(other.keySet).foreach(key => {
      output.add(key) -> this(key) * other(key)
    })

    new FeatureVector
  }

  def distance(other: FeatureVector) = {
    var sum = 0.0
    (keys ++ other.keys).foreach(key => {
      sum += Math.pow(this(key) - other(key), 2)
    })
    Math.pow(sum, 0.5)
  }

  def add(key: String, value: Double = 1.0) = {
    if (value == 0.0)
      values -= key

    values += (key -> value)
  }

  def keys = values.keys
  def keySet = values.keySet
  def apply(key: String) = values(key)

  override def toString() = values.toString()
}
