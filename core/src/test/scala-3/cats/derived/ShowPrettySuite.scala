package cats.derived

import cats.Show
import cats.laws.discipline.SerializableTests
import scala.compiletime.*

class ShowPrettySuite extends KittensSuite:
  import ShowPrettySuite.given
  import ShowPrettySuite.*
  import ADTs.*

  inline def show[A](value: A): String =
    summonInline[ShowPretty[A]].show(value)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[ShowPretty[IntTree]]))

    test(s"$instance[Foo]") {
      val value = Foo(42, Option("Hello"))
      val pretty = """
        |Foo(
        |  i = 42,
        |  b = Some(Hello)
        |)
      """.stripMargin.trim

      assertEquals(show(value), pretty)
    }

    test(s"$instance[Outer]") {
      val value = Outer(Inner(3))
      val pretty = """
        |Outer(
        |  in = Inner(
        |    i = 3
        |  )
        |)
      """.stripMargin.trim

      assertEquals(show(value), pretty)
    }

    test(s"$instance[IntTree]") {
      val value: IntTree = IntNode(IntLeaf(1), IntNode(IntNode(IntLeaf(2), IntLeaf(3)), IntLeaf(4)))
      val pretty = """
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

      assertEquals(show(value), pretty)
    }

    test(s"$instance[GenericAdt[Int]]") {
      val value: GenericAdt[Int] = GenericAdtCase(Some(1))
      val pretty = """
        |GenericAdtCase(
        |  value = Some(1)
        |)
      """.stripMargin.trim

      assertEquals(show(value), pretty)
    }

    test(s"$instance[People]") {
      val value = People("Kai", ContactInfo("303-123-4567", Address("123 1st St", "New York", "NY")))
      val pretty = """
        |People(
        |  name = Kai,
        |  contactInfo = ContactInfo(
        |    phoneNumber = 303-123-4567,
        |    address = 123 1st St New York NY
        |  )
        |)
      """.stripMargin.trim

      assertEquals(show(value), pretty)
    }

    test(s"$instance[ListField]") {
      val value = ListField("a", List(ListFieldChild(1)))
      val pretty = """
        |ListField(
        |  a = a,
        |  b = List(ListFieldChild(
        |    c = 1
        |  ))
        |)
      """.stripMargin.trim

      assertEquals(show(value), pretty)
    }

    test(s"$instance[Interleaved[Int]]") {
      val value = Interleaved(1, 2, 3, Vector(4, 5, 6), "789")
      val pretty = """
        |Interleaved(
        |  i = 1,
        |  t = 2,
        |  l = 3,
        |  tt = Vector(4, 5, 6),
        |  s = 789
        |)
      """.stripMargin.trim

      assertEquals(show(value), pretty)
    }

    test(s"$instance[Tree[Int]]") {
      val value: Tree[Int] = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
      val pretty = """
        |Node(
        |  left = Leaf(
        |    value = 1
        |  ),
        |  right = Node(
        |    left = Node(
        |      left = Leaf(
        |        value = 2
        |      ),
        |      right = Leaf(
        |        value = 3
        |      )
        |    ),
        |    right = Leaf(
        |      value = 4
        |    )
        |  )
        |)
      """.stripMargin.trim

      assertEquals(show(value), pretty)
    }

    test(s"$instance respects existing instances") {
      val value = Box(Bogus(42))
      val pretty = """
        |Box(
        |  content = Blah
        |)
      """.stripMargin.trim

      assertEquals(show(value), pretty)
    }

  end validate

  locally {
    import auto.showPretty.given
    validate("auto.showPretty")
  }

  locally {
    import semiInstances.given
    validate("semiauto.showPretty")
  }

end ShowPrettySuite

object ShowPrettySuite:
  import ADTs.*

  given Show[Address] =
    Show.show(a => List(a.street, a.city, a.state).mkString(" "))

  final case class Bogus(value: Int)
  object Bogus:
    given Show[Bogus] = Show.show(_ => "Blah")

  object semiInstances:
    given ShowPretty[Foo] = semiauto.showPretty
    given ShowPretty[Outer] = semiauto.showPretty
    given ShowPretty[IntTree] = semiauto.showPretty
    given ShowPretty[GenericAdt[Int]] = semiauto.showPretty
    given ShowPretty[People] = semiauto.showPretty
    given ShowPretty[ListFieldChild] = semiauto.showPretty
    given ShowPretty[ListField] = semiauto.showPretty
    given ShowPretty[Interleaved[Int]] = semiauto.showPretty
    given ShowPretty[Tree[Int]] = semiauto.showPretty
    given ShowPretty[Box[Bogus]] = semiauto.showPretty

  object derivedInstances:
    case class Foo(x: ADTs.Foo) derives ShowPretty
    case class Outer(x: ADTs.Outer) derives ShowPretty
    case class IntTree(x: ADTs.IntTree) derives ShowPretty
    case class People(x: ADTs.People) derives ShowPretty
    case class ListFieldChild(x: ADTs.ListFieldChild) derives ShowPretty
    case class ListField(x: ADTs.ListField) derives ShowPretty
    case class GenericAdt[A](x: ADTs.GenericAdt[A]) derives ShowPretty
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives ShowPretty
    case class Tree[A](x: ADTs.Tree[A]) derives ShowPretty
    case class BoxBogus(x: Box[Bogus]) derives ShowPretty

end ShowPrettySuite
