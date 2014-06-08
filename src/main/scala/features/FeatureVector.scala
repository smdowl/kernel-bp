package features

import scala.collection.{immutable, mutable}

class FeatureVector(values: immutable.Map[String, Double] = Map()) {

  def dot(other: FeatureVector) = {
    val keyVals = keySet.intersect(other.keySet).map(key => {
      key -> this(key) * other(key)
    })

    new FeatureVector(keyVals.toMap)
  }

  def distance(other: FeatureVector) = {
    var sum = 0.0
    (keys ++ other.keys).foreach(key => {
      sum += Math.pow(this(key) - other(key), 2)
    })
    Math.pow(sum, 0.5)
  }

  def keys = values.keys
  def keySet = values.keySet
  def apply(key: String) = values(key)
}
