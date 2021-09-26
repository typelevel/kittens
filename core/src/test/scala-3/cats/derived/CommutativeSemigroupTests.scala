package cats.derived

import cats.kernel.CommutativeSemigroup
import cats.derived.*

class CommutativeSemigroupTests { //
  case class Foo(i: Int, b: Option[Int]) derives CommutativeSemigroup
}
