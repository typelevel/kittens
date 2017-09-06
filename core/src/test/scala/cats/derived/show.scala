package cats
package derived

import cats.Show
import cats.instances.all._
import shapeless.test.illTyped
import TestDefns._

class ShowTests extends KittensSuite {


  test("Simple case classes") {
    implicit val showF : Show[Foo] = derive.show[Foo]
    val foo = Foo(42, Option("Hello"))
    val printedFoo = "Foo(i = 42, b = Some(Hello))"

    assert(foo.show == printedFoo)
  }

  test("Nested case classes auto derive inner class") {
    implicit val showO : Show[Outer] = derive.show[Outer]

    val nested = Outer(Inner(3))
    val printedNested = "Outer(in = Inner(i = 3))"

    assert(nested.show == printedNested)
  }

  test("Recursive ADTs with no type parameters") {
    import show._
    val tree: IntTree = IntNode(IntLeaf(1), IntNode(IntNode(IntLeaf(2), IntLeaf(3)), IntLeaf(4)))
    val printedTree =
      "IntNode(l = IntLeaf(t = 1), r = IntNode(l = IntNode(l = IntLeaf(t = 2), r = IntLeaf(t = 3)), r = IntLeaf(t = 4)))"

    assert(tree.show == printedTree)
  }

  test("Non recursive ADTs with type parameters") {
    implicit def showF : Show[GenericAdt[Int]] = derive.show[GenericAdt[Int]]
    val genAdt: GenericAdt[Int] = GenericAdtCase(Some(1))
    val printedGenAdt = "GenericAdtCase(v = Some(1))"

    assert(genAdt.show == printedGenAdt)
  }

  test("Recursive ADTs with type parameters are not supported") {
    import cats.derived.show._

    val tree: Tree[Int] = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
    val printedTree =
      "Node(l = Leaf(t = 1), r = Node(l = Node(l = Leaf(t = 2), r = Leaf(t = 3)), r = Leaf(t = 4)))"

    illTyped("Show[Tree[Int]]")
  }

  test("existing Show instances in scope are respected") {
    implicit def showInt: Show[Foo] = new Show[Foo] {
      def show(foo: Foo) = ""
    }

    assert(Foo(42, Option("Hello")).show == "")
  }
}
