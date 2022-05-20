package cats.derived

import cats.*
import cats.laws.discipline.{SemigroupKTests, SerializableTests}
import org.scalacheck.Arbitrary

class SemigroupKSuite extends KittensSuite {
  import SemigroupKSuite.*
  import TestDefns.*

  def testSemigroupK(context: String)(implicit
      complexProduct: SemigroupK[ComplexProduct],
      caseClassWOption: SemigroupK[CaseClassWOption],
      boxMul: SemigroupK[BoxMul]
  ): Unit = {
    checkAll(s"$context.SemigroupK[ComplexProduct]", SemigroupKTests[ComplexProduct].semigroupK[Char])
    checkAll(s"$context.SemigroupK[CaseClassWOption]", SemigroupKTests[CaseClassWOption].semigroupK[Char])
    checkAll(s"$context.SemigroupK[BoxMul]", SemigroupKTests[BoxMul].semigroupK[Char])
    checkAll(s"$context.SemigroupK is Serializable", SerializableTests.serializable(SemigroupK[ComplexProduct]))

    test(s"$context.SemigroupK respects existing instances") {
      assert(boxMul.combineK(Box(Mul[Char](5)), Box(Mul[Char](5))) == Box(Mul[Char](25)))
    }
  }

  locally {
    import auto.semigroupK.given
    summon[SemigroupK[[a] =>> Eval[Option[a]]]]
    testSemigroupK("auto")
  }

  locally {
    import semiInstances.given
    testSemigroupK("semiauto")
  }
}

object SemigroupKSuite {
  import TestDefns._

  type BoxMul[A] = Box[Mul[A]]

  object semiInstances {
    implicit val complexProduct: SemigroupK[ComplexProduct] = semiauto.semigroupK
    implicit val caseClassWOption: SemigroupK[CaseClassWOption] = semiauto.semigroupK
    implicit val boxMul: SemigroupK[BoxMul] = semiauto.semigroupK
  }

  final case class Mul[T](value: Int)
  object Mul {

    implicit def eqv[T]: Eq[Mul[T]] = Eq.by(_.value)

    implicit def arbitrary[T]: Arbitrary[Mul[T]] =
      Arbitrary(Arbitrary.arbitrary[Int].map(apply))

    implicit val semigroupK: SemigroupK[Mul] = new SemigroupK[Mul] {
      def combineK[A](x: Mul[A], y: Mul[A]) = Mul(x.value * y.value)
    }
  }
}
