package cats.derived

import alleycats._
import cats._
import cats.derived.all._

class MonoidTests { //
  case class Foo(i: Int, b: Option[String]) derives Monoid, Empty, Semigroup
}
