package kernel.parsing

import kernel.caches.EdgeBasedCache
import kernel.models.edge.Edge

trait EdgeParser {
  def buildCache(edges: Map[String, Edge], length: Int): EdgeBasedCache
}
