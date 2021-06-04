package cats.derived

import cats.Reducible
import cats.data.NonEmptyList
import cats.instances.all.*
import cats.derived.*
import cats.derived.semiauto.given

object ReducibleTests:
  case class Box[A](value: A) derives Reducible

  sealed trait OneOrMany[+A] derives Reducible
  case class One[+A](value: A) extends OneOrMany[A]
  case class Many[+A](values: NonEmptyList[A]) extends OneOrMany[A]

  sealed trait CList[A] derives Reducible
  case class COne[A](value: A) extends CList[A]
  case class CCons[A](head: A, tail: CList[A]) extends CList[A]

  case class NonEmptyTree[A](size: Int, value: A, tree: Tree[A])
  enum Tree[+A]:
    case Leaf
    case Node(left: Tree[A], value: A, right: Tree[A])

  case class Foo[A](value: A, xs: List[A])
  enum MyList[+A]:
    case Non
    case Con(v: A, r: MyList[A])

  import cats._

  DerivedFunctor[MyList]
  DerivedReducible[NonEmptyTree]

@main def run() =
  println(DerivedFunctor[ReducibleTests.MyList].map(ReducibleTests.MyList.Con(42, ReducibleTests.MyList.Non))(_.toString))
