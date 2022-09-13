package cats.derived

import cats.Show
import cats.laws.discipline.SerializableTests
import scala.compiletime.*

class ShowSuite extends KittensSuite:
  import ShowSuite.given
  import ShowSuite.*
  import TestDefns.*

  inline def show[A](value: A): String =
    summonInline[Show[A]].show(value)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Show[Foo]]))

    test(s"$instance[Foo]") {
      val value = Foo(42, Option("Hello"))
      val shown = "Foo(i = 42, b = Some(Hello))"
      assertEquals(show(value), shown)
    }

    test(s"$instance[Outer]") {
      val value = Outer(Inner(3))
      val shown = "Outer(in = Inner(i = 3))"
      assertEquals(show(value), shown)
    }

    test(s"$instance[IntTree]") {
      val value: IntTree = IntNode(IntLeaf(1), IntNode(IntNode(IntLeaf(2), IntLeaf(3)), IntLeaf(4)))
      val shown =
        "IntNode(l = IntLeaf(t = 1), r = IntNode(l = IntNode(l = IntLeaf(t = 2), r = IntLeaf(t = 3)), r = IntLeaf(t = 4)))"
      assertEquals(show(value), shown)
    }

    test(s"$instance[GenericAdt[Int]]") {
      val value: GenericAdt[Int] = GenericAdtCase(Some(1))
      val shown = "GenericAdtCase(value = Some(1))"
      assertEquals(show(value), shown)
    }

    test(s"$instance[People]") {
      val value = People("Kai", ContactInfo("303-123-4567", Address("123 1st St", "New York", "NY")))
      val shown =
        "People(name = Kai, contactInfo = ContactInfo(phoneNumber = 303-123-4567, address = 123 1st St New York NY))"
      assertEquals(show(value), shown)
    }

    test(s"$instance[ListField]") {
      val value = ListField("a", List(ListFieldChild(1)))
      val shown = "ListField(a = a, b = List(ListFieldChild(c = 1)))"
      assertEquals(show(value), shown)
    }

    test(s"$instance[Interleaved[Int]]") {
      val value = Interleaved(1, 2, 3, Vector(4, 5, 6), "789")
      val shown = "Interleaved(i = 1, t = 2, l = 3, tt = Vector(4, 5, 6), s = 789)"
      assertEquals(show(value), shown)
    }

    test(s"$instance[Tree[Int]]") {
      val value: Tree[Int] = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
      val shown =
        "Node(left = Leaf(value = 1), right = Node(left = Node(left = Leaf(value = 2), right = Leaf(value = 3)), right = Leaf(value = 4)))"
      assertEquals(show(value), shown)
    }

    test(s"$instance[EnumK0]") {
      val value: EnumK0 = EnumK0.LeafI(3)
      val shown =
        "LeafI(value = 3)"
      assertEquals(show(value), shown)
    }

    test(s"$instance respects existing instances") {
      val value = Box(Bogus(42))
      val shown = "Box(content = Blah)"
      assertEquals(show(value), shown)
    }

  end validate

  locally {
    import auto.show.given
    validate("auto.show")
  }

  locally {
    import semiInstances.given
    validate("semiauto.show")
  }

end ShowSuite

object ShowSuite:
  import TestDefns.*

  given Show[Address] =
    Show.show(a => List(a.street, a.city, a.state).mkString(" "))

  final case class Bogus(value: Int)
  object Bogus:
    given Show[Bogus] = Show.show(_ => "Blah")

  object semiInstances:
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
    given Show[EnumK0] = semiauto.show

  object derivedInstances:
    case class Foo(x: TestDefns.Foo) derives Show
    case class Outer(x: TestDefns.Outer) derives Show
    case class IntTree(x: TestDefns.IntTree) derives Show
    case class People(x: TestDefns.People) derives Show
    case class ListFieldChild(x: TestDefns.ListFieldChild) derives Show
    case class ListField(x: TestDefns.ListField) derives Show
    case class EnumK0(x: TestDefns.EnumK0) derives Show
    case class GenericAdt[A](x: TestDefns.GenericAdt[A]) derives Show
    case class Interleaved[A](x: TestDefns.Interleaved[A]) derives Show
    case class Tree[A](x: TestDefns.Tree[A]) derives Show
    case class BoxBogus(x: Box[Bogus]) derives Show

end ShowSuite
