package cats.derived

import alleycats.*
import cats.*
import cats.derived.semiauto.*

class EqTests { //
  case class Foo(i: Int, b: Option[String]) derives Eq
}
