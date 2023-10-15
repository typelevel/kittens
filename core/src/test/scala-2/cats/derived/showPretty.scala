package cats
package derived

import cats.laws.discipline.SerializableTests

class ShowPrettySuite extends KittensSuite {
  import ShowPrettySuite._
  import TestDefns._

  def testShowPretty(context: String)(implicit
      foo: ShowPretty[Foo],
      outer: ShowPretty[Outer],
      intTree: ShowPretty[IntTree],
      genericAdt: ShowPretty[GenericAdt[Int]],
      people: ShowPretty[People],
      listField: ShowPretty[ListField],
      interleaved: ShowPretty[Interleaved[Int]],
      tree: ShowPretty[Tree[Int]],
      singletons: ShowPretty[Singletons[Int]],
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

      assert(value.show == pretty)
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

      assert(value.show == pretty)
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

      assert(value.show == pretty)
    }

    test(s"$context.ShowPretty[GenericAdt[Int]]") {
      val value: GenericAdt[Int] = GenericAdtCase(Some(1))
      val pretty = """
        |GenericAdtCase(
        |  value = Some(1)
        |)
      """.stripMargin.trim

      assert(value.show == pretty)
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

      assert(value.show == pretty)
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

      assert(value.show == pretty)
    }

    test(s"$context.ShowPretty[Interleaved[Int]]") {
      val value = Interleaved(1, 2, 3, List(4, 5, 6), "789")
      val pretty = """
        |Interleaved(
        |  i = 1,
        |  t = 2,
        |  l = 3,
        |  tt = List(4, 5, 6),
        |  s = 789
        |)
      """.stripMargin.trim

      assert(value.show == pretty)
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

      assert(value.show == pretty)
    }

    test(s"$context.ShowPretty[Singletons[Int]]") {
      val value = Singletons.wrap(313)
      val pretty = """
        |Singletons(
        |  value = 313,
        |  str = Scala,
        |  sym = Symbol(fun),
        |  lng = 42,
        |  dbl = 3.14
        |)
      """.stripMargin.trim

      assert(value.show == pretty)
    }

    test(s"$context.ShowPretty respects existing instances") {
      val value = Box(Bogus(42))
      val pretty = """
        |Box(
        |  content = Blah
        |)
      """.stripMargin.trim

      assert(value.show == pretty)
    }
  }

  {
    import auto.showPretty._
    testShowPretty("auto")
  }

  {
    import cached.showPretty._
    testShowPretty("cached")
  }

  {
    import semiInstances._
    testShowPretty("semiauto")
  }
}

object ShowPrettySuite {
  import TestDefns._

  implicit val showAddress: Show[Address] = Show.show { a =>
    List(a.street, a.city, a.state).mkString(" ")
  }

  final case class Bogus(value: Int)
  object Bogus {
    implicit val show: Show[Bogus] = Show.show(_ => "Blah")
  }

  object semiInstances {
    implicit val foo: ShowPretty[Foo] = semiauto.showPretty
    implicit val outer: ShowPretty[Outer] = semiauto.showPretty
    implicit val intTree: ShowPretty[IntTree] = semiauto.showPretty
    implicit val genericAdt: ShowPretty[GenericAdt[Int]] = semiauto.showPretty
    implicit val people: ShowPretty[People] = semiauto.showPretty
    implicit val listFieldChild: ShowPretty[ListFieldChild] = semiauto.showPretty
    implicit val listField: ShowPretty[ListField] = semiauto.showPretty
    implicit val interleaved: ShowPretty[Interleaved[Int]] = semiauto.showPretty
    implicit val tree: ShowPretty[Tree[Int]] = semiauto.showPretty
    implicit val singletons: ShowPretty[Singletons[Int]] = semiauto.showPretty
    implicit val boxBogus: ShowPretty[Box[Bogus]] = semiauto.showPretty
  }
}
