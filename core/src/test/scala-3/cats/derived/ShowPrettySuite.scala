package cats
package derived

import cats.laws.discipline.SerializableTests

class ShowPrettySuite extends KittensSuite:
  import ShowPrettySuite.*
  import TestDefns.*

  def testShowPretty(context: String)(using
      foo: ShowPretty[Foo],
      outer: ShowPretty[Outer],
      intTree: ShowPretty[IntTree],
      genericAdt: ShowPretty[GenericAdt[Int]],
      people: ShowPretty[People],
      listField: ShowPretty[ListField],
      interleaved: ShowPretty[Interleaved[Int]],
      tree: ShowPretty[Tree[Int]],
      boxBogus: ShowPretty[Box[Bogus]]
  ): Unit = {
    checkAll(s"$context.ShowPretty is Serializable", SerializableTests.serializable(ShowPretty[IntTree]))

    test(s"$context.ShowPretty[Foo]") {
      val value = Foo(42, Option("Hello"))
      val pretty = """
        |Foo(
        |  i = 42,
        |  b = Some(Hello)
        |)
      """.stripMargin.trim

      assertEquals(value.show, pretty)
    }

    test(s"$context.ShowPretty[Outer]") {
      val value = Outer(Inner(3))
      val pretty = """
        |Outer(
        |  in = Inner(
        |    i = 3
        |  )
        |)
      """.stripMargin.trim

      assertEquals(value.show, pretty)
    }

    test(s"$context.ShowPretty[IntTree]") {
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

      assertEquals(value.show, pretty)
    }

    test(s"$context.ShowPretty[GenericAdt[Int]]") {
      val value: GenericAdt[Int] = GenericAdtCase(Some(1))
      val pretty = """
        |GenericAdtCase(
        |  value = Some(1)
        |)
      """.stripMargin.trim

      assertEquals(value.show, pretty)
    }

    test(s"$context.ShowPretty[People]") {
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

      assertEquals(value.show, pretty)
    }

    test(s"$context.ShowPretty[ListField]") {
      val value = ListField("a", List(ListFieldChild(1)))
      val pretty = """
        |ListField(
        |  a = a,
        |  b = List(ListFieldChild(
        |    c = 1
        |  ))
        |)
      """.stripMargin.trim

      assertEquals(value.show, pretty)
    }

    test(s"$context.ShowPretty[Interleaved[Int]]") {
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

      assertEquals(value.show, pretty)
    }

    test(s"$context.ShowPretty[Tree[Int]]") {
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

      assertEquals(value.show, pretty)
    }

    test(s"$context.ShowPretty respects existing instances") {
      val value = Box(Bogus(42))
      val pretty = """
        |Box(
        |  content = Blah
        |)
      """.stripMargin.trim

      assertEquals(value.show, pretty)
    }
  }

  locally {
    import auto.showPretty.given
    testShowPretty("auto")
  }

  locally {
    import semiInstances.given
    testShowPretty("semiauto")
  }

object ShowPrettySuite:
  import TestDefns.*

  implicit val showAddress: Show[Address] = Show.show { a =>
    List(a.street, a.city, a.state).mkString(" ")
  }

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
