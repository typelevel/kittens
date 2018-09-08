package cats
package derived

import cats.derived.TestDefns._
import cats.implicits._
import cats.laws.discipline.{FoldableTests, TraverseTests}
import org.scalacheck.Test
import org.scalatest.FreeSpec
import shapeless.test.illTyped

class TraverseSuite extends FreeSpec {

  "traverse" - {

    "passes cats traverse tests" in {
      Test.checkProperties(
        Test.Parameters.default,
        TraverseTests[IList](semi.traverse[IList]).traverse[Int, Double, String, Long, Option, Option].all
      )
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

    "auto derivation work for interleaved case class" in {
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

    "folds" - {

      "pass cats fold tests" in {
        Test.checkProperties(
          Test.Parameters.default,
          FoldableTests[IList](semi.traverse[IList]).foldable[Int, Double].all
        )
      }

      "are implemented correctly" in {
        implicit val F = semi.traverse[IList].asInstanceOf[MkTraverse[IList]]

        val iList = IList.fromSeq(List.range(1, 5))

        // just basic sanity checks
        assert(F.foldLeft(iList, "x")(_ + _) == "x1234")
        assert(F.foldRight(iList, Now("x"))((i, b) => b.map(i + _)).value == "1234x")
        assert(F.foldMap(iList)(_.toDouble) == 10)
      }

      "are stack safe" in {
        implicit val F = semi.traverse[IList]

        val n = 10000
        val llarge = IList.fromSeq(List.range(1, n))

        val expected = n * (n - 1) / 2
        val evalActual = F.foldRight(llarge, Now(0))((buff, eval) => eval.map(_ + buff))

        assert(evalActual.value == expected)
      }
    }
  }

  implicit def eqTestClass[T: Eq]: Eq[IList[T]] = semi.eq

}

private case class TestTraverse[T](i: Int, t: T, d: Double, tt: List[T], s: String)

