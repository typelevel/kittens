package cats.derived

import alleycats._
import cats._
import cats.derived.all._

class EmptyTests:
  case class Foo(i: Int, b: Option[IntTree]) derives Empty
  enum IntTree:
    case Leaf
    case Node(left: IntTree, value: Int, right: IntTree)
