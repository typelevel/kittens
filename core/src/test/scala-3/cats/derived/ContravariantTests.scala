package cats.derived

import cats.Contravariant
import cats.derived.all._
import cats.derived.all.given

class ContravariantTests {

  case class Foo[A](f: A => String) derives Contravariant

}
