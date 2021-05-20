package cats.derived

import alleycats._
import cats._
import cats.derived.all._
import cats.derived.all.given

class SemigroupKTests { //
  case class Foo[A](i: String, l: List[A]) derives SemigroupK
}
