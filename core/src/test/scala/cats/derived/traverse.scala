package cats
package derived

import cats.instances.all._
import cats.laws.discipline.{SerializableTests, TraverseTests}

class TraverseSuite extends KittensSuite {
  import TestDefns._
  import TestEqInstances._

  type OptList[A] = Option[List[A]]
  type ListSnoc[A] = List[Snoc[A]]
  type AndChar[A] = (A, Char)

  def testTraverse(context: String)(
    implicit iList: Traverse[IList],
    tree: Traverse[Tree],
    genericAdt: Traverse[GenericAdt],
    optList: Traverse[OptList],
    listSnoc: Traverse[ListSnoc],
    andChar: Traverse[AndChar],
    interleaved: Traverse[Interleaved]
  ): Unit = {
    checkAll(s"$context.Traverse[IList]", TraverseTests[IList].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$context.Traverse[Tree]", TraverseTests[Tree].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$context.Traverse[GenericAdt]", TraverseTests[GenericAdt].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$context.Traverse[OptList]", TraverseTests[OptList].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$context.Traverse[ListSnoc]", TraverseTests[ListSnoc].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$context.Traverse[AndChar]", TraverseTests[AndChar].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$context.Traverse[Interleaved]", TraverseTests[Interleaved].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$context.Traverse is Serializable", SerializableTests.serializable(Traverse[Tree]))

    val n = 10000
    val largeIList: IList[Int] = IList.fromSeq(1 until n)
    val largeSnoc: ListSnoc[Int] = Snoc.fromSeq(1 until n) :: Nil

    test(s"$context.Traverse.traverse is stack safe") {
      val actualIList = largeIList.traverse(i => Option(i + 1)).map(IList.toList)
      val actualSnoc = largeSnoc.traverse(i => Option(i + 1)).map(_.flatMap(Snoc.toList))
      val expected = Some(2 until n + 1)
      assert(actualIList == expected)
      assert(actualSnoc == expected)
    }

    test(s"$context.Traverse.foldLeft is stack safe") {
      val actualIList = largeIList.foldLeft(0)(_ + _)
      val actualSnoc = listSnoc.foldLeft(largeSnoc, 0)(_ + _)
      val expected = n * (n - 1) / 2
      assert(actualIList == expected)
      assert(actualSnoc == expected)
    }

    test(s"$context.Traverse.foldRight is stack safe") {
      val actualIList = largeIList.foldRight(Eval.Zero)((i, sum) => sum.map(_ + i))
      val actualSnoc = listSnoc.foldRight(largeSnoc, Eval.Zero)((i, sum) => sum.map(_ + i))
      val expected = n * (n - 1) / 2
      assert(actualIList.value == expected)
      assert(actualSnoc.value == expected)
    }
  }

  {
    import auto.traverse._
    testTraverse("auto")
  }

  {
    import cached.traverse._
    testTraverse("cached")
  }

  semiTests.run()

  object semiTests {
    implicit val iList: Traverse[IList] = semi.traverse
    implicit val tree: Traverse[Tree] = semi.traverse
    implicit val genericAdt: Traverse[GenericAdt] = semi.traverse
    implicit val optList: Traverse[OptList] = semi.traverse
    implicit val listSnoc: Traverse[ListSnoc] = semi.traverse
    implicit val andChar: Traverse[AndChar] = semi.traverse
    implicit val interleaved: Traverse[Interleaved] = semi.traverse
    def run(): Unit = testTraverse("semi")
  }
}

