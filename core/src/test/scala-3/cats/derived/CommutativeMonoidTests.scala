package cats.derived

import alleycats._
import cats._
import cats.kernel.CommutativeMonoid
import cats.derived.all._

class CommutativeMonoidTests { //
  case class Foo(i: Int, b: Option[Int]) derives CommutativeMonoid
}
