package cats.derived

import cats.Reducible
import cats.data.NonEmptyList
import cats.derived.all._
import cats.instances.all._

class ReducibleTests {

  case class Box[A](value: A) derives Reducible

  sealed trait OneOrMany[+A] derives Reducible
  case class One[+A](value: A) extends OneOrMany[A]
  case class Many[+A](values: NonEmptyList[A]) extends OneOrMany[A]

  sealed trait CList[A] derives Reducible
  case class COne[A](value: A) extends CList[A]
  case class CCons[A](head: A, tail: CList[A]) extends CList[A]

  case class NonEmptyTree[A](size: Int, value: A, tree: Tree[A])

  case class Foo[A](value: A, xs: List[A])
  sealed trait Tree[+A] derives Reducible
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](left: Tree[A], values: Foo[A], right: Tree[A]) extends Tree[A]
}
