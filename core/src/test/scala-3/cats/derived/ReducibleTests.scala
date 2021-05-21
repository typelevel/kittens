package cats.derived

import cats.Reducible
import cats.data.NonEmptyList
import cats.derived.all._
import cats.derived.all.given

class ReducibleTests {

  case class Box[A](value: A) derives Reducible

  sealed trait OneOrMany[+A]
  case class One[+A](value: A) extends OneOrMany[A]
  case class Many[+A](values: NonEmptyList[A]) extends OneOrMany[A]

  Reducible.derived[One]
  Reducible.derived[Many]
  Reducible.derived[OneOrMany]

  sealed trait CList[A]
  case object CNil extends CList[Nothing]
  case class CCons[A](head: A, tail: CCons[A]) extends CList[A]
}
