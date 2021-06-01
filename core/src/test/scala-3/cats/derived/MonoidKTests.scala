package cats.derived

import alleycats.*
import cats.*
import cats.derived.semiauto.*

class MonoidKTests { //
  case class Foo[A](i: String, l: List[A]) derives MonoidK
}
