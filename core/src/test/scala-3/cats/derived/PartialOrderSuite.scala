package cats.derived

import alleycats.*
import cats.*
import cats.derived.semiauto.*

class PartialOrderTests { //
  case class Foo(i: Int, b: Option[String]) derives PartialOrder
}
