package cats.derived

import alleycats.*
import alleycats.std.all.*
import cats.*
import cats.derived.*

class EmptyKTests { //
  case class Foo[A](i: String, l: List[A]) derives EmptyK
}
