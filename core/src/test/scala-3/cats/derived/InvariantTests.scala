package cats.derived

import cats.Invariant
import cats.derived.semiauto.*
import cats.derived.semiauto.given

class InvariantTests {

  case class Foo[A](bar: String, baz: Option[A]) derives Invariant

}
