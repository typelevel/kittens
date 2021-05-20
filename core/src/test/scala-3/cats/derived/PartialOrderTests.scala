package cats.derived

import alleycats._
import cats._
import cats.derived.all._

class PartialOrderTests { //
  case class Foo(i: Int, b: Option[String]) derives PartialOrder
}
