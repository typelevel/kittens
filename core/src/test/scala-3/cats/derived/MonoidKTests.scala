package cats.derived

import alleycats.*
import cats.*
import cats.derived.*

class MonoidKTests { //
  case class Foo[A](i: String, l: List[A]) derives MonoidK
}
