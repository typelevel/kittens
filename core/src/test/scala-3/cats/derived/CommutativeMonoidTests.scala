package cats.derived

import cats.kernel.CommutativeMonoid
import cats.derived.*

class CommutativeMonoidTests { //
  case class Foo(i: Int, b: Option[Int]) derives CommutativeMonoid
}
