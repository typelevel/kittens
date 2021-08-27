package cats
package derived

import cats.laws.discipline.{SerializableTests, TraverseTests}

class TraverseSuite extends KittensSuite {
  import TestDefns.*
  import TraverseSuite.*

  def testTraverse(context: String)(implicit
      iList: Traverse[IList],
      tree: Traverse[Tree],
      genericAdt: Traverse[GenericAdt],
      optList: Traverse[OptList],
      listSnoc: Traverse[ListSnoc],
      andChar: Traverse[AndChar],
      interleaved: Traverse[Interleaved]
  ): Unit = {
    checkAll(s"$context.Traverse[IList]", TraverseTests[IList].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$context.Traverse[Tree]", TraverseTests[Tree].traverse[Int, Double, String, Long, Option, Option])
    checkAll(
      s"$context.Traverse[GenericAdt]",
      TraverseTests[GenericAdt].traverse[Int, Double, String, Long, Option, Option]
    )
    checkAll(s"$context.Traverse[OptList]", TraverseTests[OptList].traverse[Int, Double, String, Long, Option, Option])
    checkAll(
      s"$context.Traverse[ListSnoc]",
      TraverseTests[ListSnoc].traverse[Int, Double, String, Long, Option, Option]
    )
    checkAll(s"$context.Traverse[AndChar]", TraverseTests[AndChar].traverse[Int, Double, String, Long, Option, Option])
    checkAll(
      s"$context.Traverse[Interleaved]",
      TraverseTests[Interleaved].traverse[Int, Double, String, Long, Option, Option]
    )
    checkAll(s"$context.Traverse is Serializable", SerializableTests.serializable(Traverse[Tree]))
  }

  {
    import auto.traverse.given
    testTraverse("auto")
  }

  {
    import semiInstances._
    testTraverse("semiauto")
  }
}

object TraverseSuite {
  import TestDefns._

  type OptList[A] = Option[List[A]]
  type ListSnoc[A] = List[Snoc[A]]
  type AndChar[A] = (A, Char)

  object semiInstances {
    implicit val iList: Traverse[IList] = semiauto.traverse
    implicit val tree: Traverse[Tree] = semiauto.traverse
    implicit val genericAdt: Traverse[GenericAdt] = semiauto.traverse
    implicit val optList: Traverse[OptList] = semiauto.traverse
    implicit val listSnoc: Traverse[ListSnoc] = semiauto.traverse
    implicit val andChar: Traverse[AndChar] = semiauto.traverse
    implicit val interleaved: Traverse[Interleaved] = semiauto.traverse
  }
}
