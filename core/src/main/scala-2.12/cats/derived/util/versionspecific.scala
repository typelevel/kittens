package cats.derived.util

import scala.util.hashing.MurmurHash3

private[derived] object VersionSpecific {
  def productSeed(x: Product): Int = MurmurHash3.productSeed
}
