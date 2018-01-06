package cats
package derived

import TestDefns._
import cats.instances.all._
import cats.syntax.show._
import shapeless.test.illTyped
import org.scalatest.FreeSpec


class ShowSuite extends FreeSpec with SerializableTest {

  "derive.show" - {
    "for simple case classes" in {
      implicit val sf = derive.show[Foo]
      val foo = Foo(42, Option("Hello"))
      val printedFoo = "Foo(i = 42, b = Some(Hello))"
      assert(foo.show == printedFoo)
    }

    "for nested case classes" in {
      implicit val so = derive.show[Outer]
      val nested = Outer(Inner(3))
      val printedNested = "Outer(in = Inner(i = 3))"
      assert(nested.show == printedNested)
    }

    "respects existing instances" in {
      import ShowSuite._
      implicit val so = derive.show[Outer]
      val printedNested = "Outer(in = Blah)"
      val nested = Outer(Inner(3))
      assert(nested.show == printedNested)
    }

    "for non-generic recursive ADTs" in {
      implicit val st = derive.show[IntTree]
      val tree: IntTree = IntNode(IntLeaf(1), IntNode(IntNode(IntLeaf(2), IntLeaf(3)), IntLeaf(4)))
      val printedTree =
        "IntNode(l = IntLeaf(t = 1), r = IntNode(l = IntNode(l = IntLeaf(t = 2), r = IntLeaf(t = 3)), r = IntLeaf(t = 4)))"
      assert(tree.show == printedTree)
    }

    "for generic non-recursive ADTs" in {
      implicit val sg = derive.show[GenericAdt[Int]]
      val genAdt: GenericAdt[Int] = GenericAdtCase(Some(1))
      val printedGenAdt = "GenericAdtCase(v = Some(1))"
      assert(genAdt.show == printedGenAdt)
    }

    "does not support generic recursive ADTs" in {
      illTyped("derive.show[Tree[Int]]")
    }

    "for deep type hierarchies" in {
      derive.show[Top]
      derive.show[People]
    }

    "for deep type hierarchies respects existing instances" in {
      implicit val sAdd : Show[Address] = Show.show { address =>
        address.street + " " + address.city + " " + address.state
      }

      assert(derive.show[People].show(People(name = "Kai",
        contactInfo = ContactInfo(
          phoneNumber = "303-123-4567",
          address = Address(
            street = "123 1st St",
            city = "New York", state = "NY") ))) ==
        "People(name = Kai, contactInfo = ContactInfo(phoneNumber = 303-123-4567, address = 123 1st St New York NY))")
    }
  }

  "derived.show" - {
    import derived.show._

    "respects existing instances in companion objects" in {
      import ShowSuite._
      val b = B(A(42))
      val printedB = "B(a = an A)"
      assert(b.show == printedB)
    }

    "resolves across existing generic instances" in {
      val genAdt: GenericAdt[Foo] = GenericAdtCase(None)
      // derive.show prints None.type
      val printedGenAdt = "GenericAdtCase(v = None)"
      assert(genAdt.show == printedGenAdt)
    }

//    "takes priority over fallback instances" in {
//      import ShowSuite._
//      assert(Foo(33, Some("kitten")).show == "Foo(i = 33, v = Some(kitten))")
//    }
  }
}

object ShowSuite {
  implicit def showInner: Show[Inner] = Show.show(_ => "Blah")

//  implicit def showFallback[A]: MkShow.Default[A] =
//    new MkShow[A](Show.fromToString) with Priority.Default

  case class B(a: A)
  case class A(i: Int)
  object A {
    implicit val showA: Show[A] = Show.show(_ => "an A")
  }
}
