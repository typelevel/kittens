package cats.derived.util

import scala.util.hashing.MurmurHash3

private[derived] object VersionSpecific {
  type Lazy[+A] = shapeless.Lazy[A]
  type OrElse[+A, +B] = shapeless.OrElse[A, B]
  def productSeed(x: Product): Int = MurmurHash3.productSeed
}
