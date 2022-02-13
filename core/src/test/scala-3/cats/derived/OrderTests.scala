package cats.derived

import alleycats.*
import cats.*
import cats.derived.*

class OrderTests { //
  case class Foo(i: Int, b: Option[String]) derives Order
}
