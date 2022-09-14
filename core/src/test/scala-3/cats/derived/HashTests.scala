package cats.derived

import alleycats.*
import cats.*
import cats.derived.*

class HashTests { //
  case class Foo(i: Int, b: Option[String]) derives Hash
}
