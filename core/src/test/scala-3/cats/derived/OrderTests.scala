package cats.derived

import alleycats.*
import cats.*
import cats.derived.semiauto.*

class OrderTests { //
  case class Foo(i: Int, b: Option[String]) derives Order
}
