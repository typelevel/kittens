package cats.derived

import cats.{Eq, Traverse}
import cats.laws.discipline.{SerializableTests, TraverseTests}
import org.scalacheck.Arbitrary

import scala.compiletime.*

class TraverseSuite extends KittensSuite:
  import TestDefns.*
  import TraverseSuite.*

  inline def traverseTests[F[_]]: TraverseTests[F] =
    TraverseTests[F](summonInline)

  inline def testTraverse(inline context: String): Unit =
    checkAll(s"$context.Traverse[IList]", traverseTests[IList].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$context.Traverse[Tree]", traverseTests[Tree].traverse[Int, Double, String, Long, Option, Option])
    checkAll(
      s"$context.Traverse[GenericAdt]",
      traverseTests[GenericAdt].traverse[Int, Double, String, Long, Option, Option]
    )
    checkAll(s"$context.Traverse[OptList]", traverseTests[OptList].traverse[Int, Double, String, Long, Option, Option])
    checkAll(
      s"$context.Traverse[ListSnoc]",
      traverseTests[ListSnoc].traverse[Int, Double, String, Long, Option, Option]
    )
    checkAll(s"$context.Traverse[AndChar]", traverseTests[AndChar].traverse[Int, Double, String, Long, Option, Option])
    checkAll(
      s"$context.Traverse[Interleaved]",
      traverseTests[Interleaved].traverse[Int, Double, String, Long, Option, Option]
    )
    checkAll(s"$context.Traverse is Serializable", SerializableTests.serializable(summonInline[Traverse[Tree]]))

  locally {
    import auto.traverse.given
    testTraverse("auto")
  }

  locally {
    import semiInstances.given
    testTraverse("semiauto")
  }

end TraverseSuite

object TraverseSuite:
  import TestDefns.*

  type OptList[A] = Option[List[A]]
  type ListSnoc[A] = List[Snoc[A]]
  type AndChar[A] = (A, Char)

  object semiInstances:
    given Traverse[IList] = semiauto.traverse
    given Traverse[Tree] = semiauto.traverse
    given Traverse[GenericAdt] = semiauto.traverse
    given Traverse[OptList] = semiauto.traverse
    given Traverse[ListSnoc] = semiauto.traverse
    given Traverse[AndChar] = semiauto.traverse
    given Traverse[Interleaved] = semiauto.traverse

end TraverseSuite
