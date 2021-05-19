package cats.derived

import alleycats._
import cats._
import cats.kernel.CommutativeSemigroup
import cats.derived.all._

class CommutativeSemigroupTests { //
  case class Foo(i: Int, b: Option[Int]) derives CommutativeSemigroup
}
