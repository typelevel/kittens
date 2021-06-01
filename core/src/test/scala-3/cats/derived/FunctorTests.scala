package cats.derived

import cats.Functor
import cats.derived.semiauto.*

class FunctorTests {

  case class Box[A](value: A) derives Functor

  sealed trait Maybe[+A] derives Functor
  case object Nufin extends Maybe[Nothing]
  case class Just[A](value: A) extends Maybe[A]

  sealed trait CList[A] derives Functor
  case object CNil extends CList[Nothing]
  case class CCons[A](head: A, tail: CList[A]) extends CList[A]
}
