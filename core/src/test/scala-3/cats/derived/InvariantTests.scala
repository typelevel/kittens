package cats.derived

import cats.Invariant
import cats.derived.all._
import cats.derived.all.given

class InvariantTests {

  case class Foo[A](bar: String, baz: Option[A]) derives Invariant

}
