package cats.derived

import alleycats.*
import cats.*
import cats.kernel.CommutativeMonoid
import cats.derived.semiauto.*

class CommutativeMonoidTests { //
  case class Foo(i: Int, b: Option[Int]) derives CommutativeMonoid
}
