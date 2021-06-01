package cats.derived

import alleycats.*
import cats.*
import cats.derived.semiauto.*

class ShowTests { //
  case class Foo(i: Int, b: Option[String]) derives Show
}
