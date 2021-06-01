package cats.derived

import alleycats.*
import cats.*
import cats.derived.semiauto.*

class EmptyTests:
  case class Foo(i: Int, b: IntTree) derives Empty
  enum IntTree:
    case Leaf
    case Node(left: IntTree, value: Int, right: IntTree)
