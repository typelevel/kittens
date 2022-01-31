package cats
package derived

import cats.laws.discipline.SerializableTests
import scala.compiletime.*

class ShowSuite extends KittensSuite {
  import ShowSuite._
  import TestDefns._

  def testShow(context: String)(implicit
      foo: Show[Foo],
      outer: Show[Outer],
      intTree: Show[IntTree],
      genericAdt: Show[GenericAdt[Int]],
      people: Show[People],
      listField: Show[ListField],
      interleaved: Show[Interleaved[Int]],
      tree: Show[Tree[Int]],
      boxBogus: Show[Box[Bogus]]
  ): Unit = {
    checkAll(s"$context.Show is Serializable", SerializableTests.serializable(summonInline[Show[Foo]]))

    test(s"$context.Show[Foo]") {
      val value = Foo(42, Option("Hello"))
      val shown = "Foo(i = 42, b = Some(Hello))"
      assertEquals(value.show, shown)
    }

    test(s"$context.Show[Outer]") {
      val value = Outer(Inner(3))
      val shown = "Outer(in = Inner(i = 3))"
      assertEquals(value.show, shown)
    }

    test(s"$context.Show[IntTree]") {
      val value: IntTree = IntNode(IntLeaf(1), IntNode(IntNode(IntLeaf(2), IntLeaf(3)), IntLeaf(4)))
      val shown =
        "IntNode(l = IntLeaf(t = 1), r = IntNode(l = IntNode(l = IntLeaf(t = 2), r = IntLeaf(t = 3)), r = IntLeaf(t = 4)))"
      assertEquals(value.show, shown)
    }

    test(s"$context.Show[GenericAdt[Int]]") {
      val value: GenericAdt[Int] = GenericAdtCase(Some(1))
      val shown = "GenericAdtCase(value = Some(1))"
      assertEquals(value.show, shown)
    }

    test(s"$context.Show[People]") {
      val value = People("Kai", ContactInfo("303-123-4567", Address("123 1st St", "New York", "NY")))
      val shown =
        "People(name = Kai, contactInfo = ContactInfo(phoneNumber = 303-123-4567, address = 123 1st St New York NY))"
      assertEquals(value.show, shown)
    }

    test(s"$context.Show[ListField]") {
      val value = ListField("a", List(ListFieldChild(1)))
      val shown = "ListField(a = a, b = List(ListFieldChild(c = 1)))"
      assertEquals(value.show, shown)
    }

    test(s"$context.Show[Interleaved[Int]]") {
      val value = Interleaved(1, 2, 3, Vector(4, 5, 6), "789")
      val shown = "Interleaved(i = 1, t = 2, l = 3, tt = Vector(4, 5, 6), s = 789)"
      assertEquals(value.show, shown)
    }

    test(s"$context.Show[Tree[Int]]") {
      val value: Tree[Int] = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
      val shown =
        "Node(left = Leaf(value = 1), right = Node(left = Node(left = Leaf(value = 2), right = Leaf(value = 3)), right = Leaf(value = 4)))"
      assertEquals(value.show, shown)
    }

    test(s"$context.Show respects existing instances") {
      val value = Box(Bogus(42))
      val shown = "Box(content = Blah)"
      assertEquals(value.show, shown)
    }
  }

  locally {
    import auto.show.given
    testShow("auto")
  }

  locally {
    import semiInstances.given
    testShow("semiauto")
  }
}

object ShowSuite {
  import TestDefns._

  implicit val showAddress: Show[Address] = Show.show { a =>
    List(a.street, a.city, a.state).mkString(" ")
  }

  final case class Bogus(value: Int)
  object Bogus {
    given Show[Bogus] = Show.show(_ => "Blah")
  }

  object semiInstances {
    given Show[Foo] = semiauto.show
    given Show[Outer] = semiauto.show
    given Show[IntTree] = semiauto.show
    given Show[GenericAdt[Int]] = semiauto.show
    given Show[People] = semiauto.show
    given Show[ListFieldChild] = semiauto.show
    given Show[ListField] = semiauto.show
    given Show[Interleaved[Int]] = semiauto.show
    given Show[Tree[Int]] = semiauto.show
    given Show[Box[Bogus]] = semiauto.show
  }
}
