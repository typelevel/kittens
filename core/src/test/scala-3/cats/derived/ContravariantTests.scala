package cats.derived

import cats.Contravariant
import cats.derived.given

class ContravariantTests {

  case class Foo[A](f: A => String) derives Contravariant

}
