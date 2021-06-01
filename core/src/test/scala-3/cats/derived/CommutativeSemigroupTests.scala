package cats.derived

import alleycats.*
import cats.*
import cats.kernel.CommutativeSemigroup
import cats.derived.semiauto.*

class CommutativeSemigroupTests { //
  case class Foo(i: Int, b: Option[Int]) derives CommutativeSemigroup
}
