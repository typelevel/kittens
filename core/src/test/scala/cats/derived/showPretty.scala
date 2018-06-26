package cats
package derived

import cats.Show
import cats.instances.all._
import shapeless.test.illTyped
import TestDefns._

class ShowPrettyTests extends KittensSuite {
  test("Simple case classes") {
    implicit val sf = semi.showPretty[Foo]
    val foo = Foo(42, Option("Hello"))
    val printedFoo =
      """
        |Foo(
        |  i = 42,
        |  b = Some(Hello)
        |)
      """.stripMargin.trim

    assert(foo.show == printedFoo)
  }

  test("Nested case classes auto derive inner class") {
    implicit val so = semi.showPretty[Outer]

    val nested = Outer(Inner(3))
    val printedNested =
      """
        |Outer(
        |  in = Inner(
        |    i = 3
        |  )
        |)
      """.stripMargin.trim

    assert(nested.show == printedNested)
  }

  test("respect defined instance") {
    import InnerInstance._
    implicit val so = semi.showPretty[Outer]

    val printedNested =
      """
        |Outer(
        |  in = Blah
        |)
      """.stripMargin.trim

    val nested = Outer(Inner(3))

    assert(nested.show == printedNested)
  }

  test("respect defined instance with full auto derivation") {
    import InnerInstance._
    import auto.showPretty._

    val printedNested =
      """
        |Outer(
        |  in = Blah
        |)
      """.stripMargin.trim

    val nested = Outer(Inner(3))

    assert(nested.show == printedNested)
  }

  test("Recursive ADTs with no type parameters") {
    implicit val st = semi.showPretty[IntTree]

    val tree: IntTree = IntNode(IntLeaf(1), IntNode(IntNode(IntLeaf(2), IntLeaf(3)), IntLeaf(4)))
    val printedTree =
      """
        |IntNode(
        |  l = IntLeaf(
        |    t = 1
        |  ),
        |  r = IntNode(
        |    l = IntNode(
        |      l = IntLeaf(
        |        t = 2
        |      ),
        |      r = IntLeaf(
        |        t = 3
        |      )
        |    ),
        |    r = IntLeaf(
        |      t = 4
        |    )
        |  )
        |)
      """.stripMargin.trim

    assert(tree.show == printedTree)
  }

  test("Non recursive ADTs with type parameters") {
    implicit val sg = {
      import auto.showPretty._
      semi.showPretty[GenericAdt[Int]]
    }

    val genAdt: GenericAdt[Int] = GenericAdtCase(Some(1))
    val printedGenAdt =
      """
        |GenericAdtCase(
        |  v = Some(1)
        |)
      """.stripMargin.trim

    assert(genAdt.show == printedGenAdt)
  }

  test("Recursive ADTs with type parameters are not supported") {
    import auto.showPretty._
    val tree: Tree[Int] = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
    illTyped("Show[Tree[Int]]")
  }

  test("Deep type hierarchy") {
    semi.showPretty[Top]
    semi.showPretty[People]
  }

  test("Deep type hierarchy respect existing instance") {
    implicit val sAdd : Show[Address] = new Show[Address] {
      def show(t: Address) = t.street + " " + t.city + " " + t.state
    }

    val printed =
      """
        |People(
        |  name = Kai,
        |  contactInfo = ContactInfo(
        |    phoneNumber = 303-123-4567,
        |    address = 123 1st St New York NY
        |  )
        |)
      """.stripMargin.trim

    assert(semi.showPretty[People].show(People(name = "Kai",
      contactInfo = ContactInfo(
        phoneNumber = "303-123-4567",
        address = Address(
          street = "123 1st St",
          city = "New York", state = "NY") ))) == printed)
  }

  test("Deep type hierarchy respect existing instance in full auto derivation") {
    implicit val sAdd : Show[Address] = new Show[Address] {
      def show(t: Address) = t.street + " " + t.city + " " + t.state
    }

    val printed =
      """
        |People(
        |  name = Kai,
        |  contactInfo = ContactInfo(
        |    phoneNumber = 303-123-4567,
        |    address = 123 1st St New York NY
        |  )
        |)
      """.stripMargin.trim

    import auto.showPretty._
    assert(People(name = "Kai",
      contactInfo = ContactInfo(
        phoneNumber = "303-123-4567",
        address = Address(
          street = "123 1st St",
          city = "New York", state = "NY") )).show == printed)
  }

  test("semi-auto derivation respect existing instance") {
    implicit val lifShow: Show[ListField] = {
      import auto.showPretty._
      semi.showPretty
    }

    assert(ListField(a ="a", b = List(ListFieldChild(c = 1))).show ==
      """
        |ListField(
        |  a = a,
        |  b = List(ListFieldChild(
        |    c = 1
        |  ))
        |)
      """.stripMargin.trim
    )
  }
  test("auto derivation respect existing instance") {
    import auto.showPretty._

    assert(ListField(a ="a", b = List(ListFieldChild(c = 1))).show ==
      """
        |ListField(
        |  a = a,
        |  b = List(ListFieldChild(
        |    c = 1
        |  ))
        |)
      """.stripMargin.trim
    )
  }
}
