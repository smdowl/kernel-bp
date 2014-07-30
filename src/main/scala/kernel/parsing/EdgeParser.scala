package kernel.parsing

import kernel.caches.EdgeBasedCache
import kernel.kernels.Kernel
import kernel.models.edge.Edge

abstract class EdgeParser(val kernel: Kernel) {
  def buildCache(edges: Map[String, Edge], length: Int): EdgeBasedCache
}