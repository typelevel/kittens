package cats.derived

import alleycats._
import alleycats.std.all._
import cats._
import cats.derived.all._
import cats.derived.all.given

class EmptyKTests { //
  case class Foo[A](i: String, l: List[A]) derives EmptyK
}
