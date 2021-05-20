package cats.derived

import cats.Foldable
import cats.derived.all._
import cats.derived.all.given

class FoldableTests {

  case class Box[A](value: A) derives Foldable

  sealed trait Maybe[+A] derives Foldable
  case object Nufin extends Maybe[Nothing]
  case class Just[A](value: A) extends Maybe[A]

  sealed trait CList[A] derives Foldable
  case object CNil extends CList[Nothing]
  case class CCons[A](head: A, tail: CCons[A]) extends CList[A]
}
