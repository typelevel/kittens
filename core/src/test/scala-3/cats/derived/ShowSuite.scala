package cats.derived

import cats.Show
import cats.laws.discipline.SerializableTests

import scala.annotation.unused
import scala.compiletime.*

class ShowSuite extends KittensSuite:
  import ShowSuite.given
  import ShowSuite.*
  import ADTs.*

  inline def show[A](value: A): String =
    summonInline[Show[A]].show(value)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Show[Foo]]))

    test(s"$instance[Foo]"):
      val value = Foo(42, Option("Hello"))
      val shown = "Foo(i = 42, b = Some(Hello))"
      assertEquals(show(value), shown)

    test(s"$instance[Outer]"):
      val value = Outer(Inner(3))
      val shown = "Outer(in = Inner(i = 3))"
      assertEquals(show(value), shown)

    test(s"$instance[IntTree]"):
      val value: IntTree = IntNode(IntLeaf(1), IntNode(IntNode(IntLeaf(2), IntLeaf(3)), IntLeaf(4)))
      val shown =
        "IntNode(l = IntLeaf(t = 1), r = IntNode(l = IntNode(l = IntLeaf(t = 2), r = IntLeaf(t = 3)), r = IntLeaf(t = 4)))"
      assertEquals(show(value), shown)

    test(s"$instance[GenericAdt[Int]]"):
      val value: GenericAdt[Int] = GenericAdtCase(Some(1))
      val shown = "GenericAdtCase(value = Some(1))"
      assertEquals(show(value), shown)

    test(s"$instance[People]"):
      val value = People("Kai", ContactInfo("303-123-4567", Address("123 1st St", "New York", "NY")))
      val shown =
        "People(name = Kai, contactInfo = ContactInfo(phoneNumber = 303-123-4567, address = 123 1st St New York NY))"
      assertEquals(show(value), shown)

    test(s"$instance[ListField]"):
      val value = ListField("a", List(ListFieldChild(1)))
      val shown = "ListField(a = a, b = List(ListFieldChild(c = 1)))"
      assertEquals(show(value), shown)

    test(s"$instance[Interleaved[Int]]"):
      val value = Interleaved(1, 2, 3, Vector(4, 5, 6), "789")
      val shown = "Interleaved(i = 1, t = 2, l = 3, tt = Vector(4, 5, 6), s = 789)"
      assertEquals(show(value), shown)

    test(s"$instance[Tree[Int]]"):
      val value: Tree[Int] = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
      val shown =
        "Node(left = Leaf(value = 1), right = Node(left = Node(left = Leaf(value = 2), right = Leaf(value = 3)), right = Leaf(value = 4)))"
      assertEquals(show(value), shown)

    test(s"$instance[Singletons[Int]]"):
      val value = Singletons(313)
      val shown = "Singletons(value = 313, str = Scala, lng = 42, dbl = 3.14)"
      assertEquals(show(value), shown)

    test(s"$instance[EnumK0]"):
      val value: EnumK0 = EnumK0.LeafI(3)
      val shown = "LeafI(value = 3)"
      assertEquals(show(value), shown)

    test(s"$instance respects existing instances"):
      val value = Box(Bogus(42))
      val shown = "Box(content = Blah)"
      assertEquals(show(value), shown)

  end validate

  locally:
    import auto.show.given
    validate("auto.show")

  locally:
    import semiInstances.given
    validate("semiauto.show")

  locally:
    import strictInstances.given
    validate("strict.semiauto.show")
    testNoInstance("strict.semiauto.show", "Top")

end ShowSuite

object ShowSuite:
  import ADTs.*

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
    given Show[Singletons[Int]] = semiauto.show
    given Show[Box[Bogus]] = semiauto.show
    given Show[EnumK0] = semiauto.show

  object strictInstances:
    @unused given [A <: Singleton: ValueOf]: Show[A] = Show.fromToString
    given Show[Foo] = strict.semiauto.show
    given Show[Inner] = strict.semiauto.show
    given Show[Outer] = strict.semiauto.show
    given Show[IntTree] = strict.semiauto.show
    given Show[GenericAdt[Int]] = strict.semiauto.show
    given Show[ContactInfo] = strict.semiauto.show
    given Show[People] = strict.semiauto.show
    given Show[ListFieldChild] = strict.semiauto.show
    given Show[ListField] = strict.semiauto.show
    given Show[Interleaved[Int]] = strict.semiauto.show
    given Show[Tree[Int]] = strict.semiauto.show
    given Show[Singletons[Int]] = strict.semiauto.show
    given Show[Box[Bogus]] = strict.semiauto.show
    given Show[EnumK0] = strict.semiauto.show

  object derivedInstances:
    case class Foo(x: ADTs.Foo) derives Show
    case class Outer(x: ADTs.Outer) derives Show
    case class IntTree(x: ADTs.IntTree) derives Show
    case class People(x: ADTs.People) derives Show
    case class ListFieldChild(x: ADTs.ListFieldChild) derives Show
    case class ListField(x: ADTs.ListField) derives Show
    case class EnumK0(x: ADTs.EnumK0) derives Show
    case class GenericAdt[A](x: ADTs.GenericAdt[A]) derives Show
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives Show
    case class Tree[A](x: ADTs.Tree[A]) derives Show
    case class Singletons[A](x: ADTs.Singletons[A]) derives Show
    case class BoxBogus(x: Box[Bogus]) derives Show

end ShowSuite
