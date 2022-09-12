package cats.derived

import alleycats.*
import cats.*
import cats.derived.*

class EqTests { //
  case class Foo(i: Int, b: Option[String]) derives Eq
}
