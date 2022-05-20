package cats.derived

import alleycats.*
import cats.*
import cats.derived.*

class SemigroupKTests { //
  case class Foo[A](i: String, l: List[A]) derives SemigroupK
}
