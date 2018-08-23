package cats
package derived

import cats.derived.TestDefns._
import cats.implicits._
import cats.laws.discipline.TraverseTests
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FreeSpec
import shapeless.test.illTyped

class TraverseSuite extends FreeSpec {

  "traverse" - {

    "passes cats traverse tests" in {
      implicit def genTestClass[T: Arbitrary]: Arbitrary[TestTraverse[T]] = Arbitrary {
        for {
          i <- Gen.posNum[Int]
          t <- arbitrary[T]
          d <- Gen.posNum[Double]
          tt <- Gen.listOf(arbitrary[T])
          s <- Gen.alphaStr
        } yield TestTraverse[T](i, t, d, tt, s)
      }

      implicit def eqTestClass[T: Eq]: Eq[TestTraverse[T]] = semi.eq

      TraverseTests[TestTraverse](semi.traverse[TestTraverse]).traverse[Int, Double, String, Long, Option, Option].all.check()
    }

    "derives an instance for" - {
      // occasional map, as a special case of traverse[Id,_], is just fine and is easier to test
      // the laws were checked in the previous test

      "for a Tree" in {
        implicit val F = semi.traverse[Tree]

        val tree: Tree[String] =
          Node(
            Leaf("12"),
            Node(
              Leaf("3"),
              Leaf("4")
            )
          )

        val expected: List[Tree[Char]] = List(
          Node(
            Leaf('1'),
            Node(
              Leaf('3'),
              Leaf('4')
            )
          ),
          Node(
            Leaf('2'),
            Node(
              Leaf('3'),
              Leaf('4')
            )
          )
        )

        assert(tree.traverse(_.toCharArray.toList) == expected)
      }

      "for a nested List[List[_]] (with alias)" in {
        illTyped("derive.traverse[λ[t => List[List[t]]]]")
        type LList[T] = List[List[T]]
        val F = semi.traverse[LList]

        val l = List(List(1), List(2, 3), List(4, 5, 6), List(), List(7))
        val expected = List(List(2), List(3, 4), List(5, 6, 7), List(), List(8))

        assert(F.map(l)(_ + 1) == expected)
      }

      "for a pair on the left (with alias)" in {
        illTyped("derive.traverse[(?, String)]")

        def F[R]: Traverse[(?, R)] = {
          type Pair[L] = (L, R)
          semi.traverse[Pair]
        }

        val pair = (42, "shapeless")
        assert(F[String].map(pair)(_ / 2) == (21, "shapeless"))
      }

      "for a pair on the right" in {
        def F[L]: Traverse[(L, ?)] = semi.traverse[(L, ?)]

        val pair = (42, "shapeless")
        assert(F[Int].map(pair)(_.length) == (42, 9))
      }

    }

    "respects existing instances for a generic ADT " in {
      implicit val F = semi.traverse[GenericAdt]
      val adt: GenericAdt[Int] = GenericAdtCase(Some(2))
      assert(adt.map(_ + 1) == GenericAdtCase(Some(3)))
    }

    "is stack safe" in {
      implicit val F = semi.traverse[IList]

      val llarge = List.range(1, 10000)
      val large = IList.fromSeq(llarge)

      val actual = large.traverse[Option, Int](i => Option(i + 1)).map(IList.toList)
      val expected = Option(llarge.map(_ + 1))

      assert(actual == expected)
    }

    "auto derivation work" in {
      import cats.derived.auto.traverse._

      val testInstance = TestTraverse(1, "ab", 2.0, List("cd"), "3")

      val expected = List(
        TestTraverse(1, 'a', 2.0, List('c'), "3"),
        TestTraverse(1, 'a', 2.0, List('d'), "3"),
        TestTraverse(1, 'b', 2.0, List('c'), "3"),
        TestTraverse(1, 'b', 2.0, List('d'), "3")
      )
      val actual = testInstance.traverse(_.toCharArray.toList)

      assert(actual == expected)
    }
  }
}

private case class TestTraverse[T](i: Int, t: T, d: Double, tt: List[T], s: String)

