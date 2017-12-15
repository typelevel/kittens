package cats
package derived

import cats.Show
import cats.instances.all._
import shapeless.test.illTyped
import TestDefns._

class ShowTests extends KittensSuite {


  test("Simple case classes") {
    implicit val sf = derive.show[Foo]
    val foo = Foo(42, Option("Hello"))
    val printedFoo = "Foo(i = 42, b = Some(Hello))"

    assert(foo.show == printedFoo)
  }

  test("Nested case classes auto derive inner class") {
    implicit val so = derive.show[Outer]

    val nested = Outer(Inner(3))
    val printedNested = "Outer(in = Inner(i = 3))"

    assert(nested.show == printedNested)
  }

  test("respect defined instance") {
    import InnerInstance._
    implicit val so = derive.show[Outer]

    val printedNested = "Outer(in = Blah)"
    val nested = Outer(Inner(3))

    assert(nested.show == printedNested)
  }

  test("Recursive ADTs with no type parameters") {
    implicit val st = derive.show[IntTree]

    val tree: IntTree = IntNode(IntLeaf(1), IntNode(IntNode(IntLeaf(2), IntLeaf(3)), IntLeaf(4)))
    val printedTree =
      "IntNode(l = IntLeaf(t = 1), r = IntNode(l = IntNode(l = IntLeaf(t = 2), r = IntLeaf(t = 3)), r = IntLeaf(t = 4)))"

    assert(tree.show == printedTree)
  }

  test("Non recursive ADTs with type parameters") {
    implicit val sg = derive.show[GenericAdt[Int]]
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

  test("Deep type hierarchy") {
    derive.show[Top]
    derive.show[People]
  }

  test("Deep type hierarchy respect existing instance") {
    implicit val sAdd : Show[Address] = new Show[Address] {
      def show(t: Address) = t.street + " " + t.city + " " + t.state
    }
    assert(derive.show[People].show(People(name = "Kai",
      contactInfo = ContactInfo(
        phoneNumber = "303-123-4567",
        address = Address(
          street = "123 1st St",
          city = "New York", state = "NY") ))) == "People(name = Kai, contactInfo = ContactInfo(phoneNumber = 303-123-4567, address = 123 1st St New York NY))")
  }

}

object InnerInstance {
  implicit def showInner: Show[Inner] = new Show[Inner]{
    def show(t: Inner): String = "Blah"
  }
}
