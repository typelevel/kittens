package cats.derived

import alleycats._
import cats._
import cats.derived.all._

class ShowTests { //
  case class Foo(i: Int, b: Option[String]) derives Show
}
