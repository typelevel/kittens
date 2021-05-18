package cats.derived

import alleycats._
import cats._
import cats.derived.all._

class EqTests { //
  case class Foo(i: Int, b: Option[String]) derives Eq
}
