package cats.derived

import cats.Traverse
import cats.derived.all._

class TraverseTests {

  case class Box[A](value: A) derives Traverse

  sealed trait Maybe[+A] derives Traverse
  case object Nufin extends Maybe[Nothing]
  case class Just[A](value: A) extends Maybe[A]

  sealed trait CList[A] derives Traverse
  case object CNil extends CList[Nothing]
  case class CCons[A](head: A, tail: CList[A]) extends CList[A]
}
