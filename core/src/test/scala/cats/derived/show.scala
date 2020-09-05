package cats
package derived
import cats.laws.discipline.SerializableTests
import shapeless.test.illTyped

class ShowSuite extends KittensSuite {
  import ShowSuite._
  import TestDefns._

  def testShow(context: String)(
    implicit foo: Show[Foo],
    outer: Show[Outer],
    intTree: Show[IntTree],
    genericAdt: Show[GenericAdt[Int]],
    people: Show[People],
    listField: Show[ListField],
    interleaved: Show[Interleaved[Int]],
    boxBogus: Show[Box[Bogus]]
  ): Unit = {
    checkAll(s"$context.Show is Serializable", SerializableTests.serializable(Show[IntTree]))

    test(s"$context.Show[Foo]") {
      val value = Foo(42, Option("Hello"))
      val shown = "Foo(i = 42, b = Some(Hello))"
      assert(value.show == shown)
    }

    test(s"$context.Show[Outer]") {
      val value = Outer(Inner(3))
      val shown = "Outer(in = Inner(i = 3))"
      assert(value.show == shown)
    }

    test(s"$context.Show[IntTree]") {
      val value: IntTree = IntNode(IntLeaf(1), IntNode(IntNode(IntLeaf(2), IntLeaf(3)), IntLeaf(4)))
      val shown = "IntNode(l = IntLeaf(t = 1), r = IntNode(l = IntNode(l = IntLeaf(t = 2), r = IntLeaf(t = 3)), r = IntLeaf(t = 4)))"
      assert(value.show == shown)
    }

    test(s"$context.Show[GenericAdt[Int]]") {
      val value: GenericAdt[Int] = GenericAdtCase(Some(1))
      val shown = "GenericAdtCase(value = Some(1))"
      assert(value.show == shown)
    }

    test(s"$context.Show[People]") {
      val value = People("Kai", ContactInfo("303-123-4567", Address("123 1st St", "New York", "NY")))
      val shown = "People(name = Kai, contactInfo = ContactInfo(phoneNumber = 303-123-4567, address = 123 1st St New York NY))"
      assert(value.show == shown)
    }

    test(s"$context.Show[ListField]") {
      val value = ListField("a", List(ListFieldChild(1)))
      val shown = "ListField(a = a, b = List(ListFieldChild(c = 1)))"
      assert(value.show == shown)
    }

    test(s"$context.Show[Interleaved[Int]]") {
      val value = Interleaved(1, 2, 3, List(4, 5, 6), "789")
      val shown = "Interleaved(i = 1, t = 2, l = 3, tt = List(4, 5, 6), s = 789)"
      assert(value.show == shown)
    }

    test(s"$context.Show respects existing instances") {
      val value = Box(Bogus(42))
      val shown = "Box(content = Blah)"
      assert(value.show == shown)
    }
  }

  {
    import auto.show._
    testShow("auto")
    illTyped("Show[Tree[Int]]")
  }

  {
    import cached.show._
    testShow("cached")
    illTyped("Show[Tree[Int]]")
  }

  {
    import semiInstances._
    illTyped("semi.show[Tree[Int]]")
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
    implicit val show: Show[Bogus] = Show.show(_ => "Blah")
  }

  object semiInstances {
    implicit val foo: Show[Foo] = semiauto.show
    implicit val outer: Show[Outer] = semiauto.show
    implicit val intTree: Show[IntTree] = semiauto.show
    implicit val genericAdt: Show[GenericAdt[Int]] = semiauto.show
    implicit val people: Show[People] = semiauto.show
    implicit val listFieldChild: Show[ListFieldChild] = semiauto.show
    implicit val listField: Show[ListField] = semiauto.show
    implicit val interleaved: Show[Interleaved[Int]] = semiauto.show
    implicit val boxBogus: Show[Box[Bogus]] = semiauto.show
  }
}
