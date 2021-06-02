package cats.derived

import alleycats._
import cats._
import cats.derived.all._

object EmptyTests:
  case class Foo(i: Int, b: IntTree) derives Empty
  enum IntTree:
    case Leaf
    case Node(left: IntTree, value: Int, right: IntTree)

  @main def test() =
    println(Empty[Foo].empty)
