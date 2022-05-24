package cats.derived

import cats.*
import cats.laws.discipline.{SemigroupKTests, SerializableTests}
import org.scalacheck.Arbitrary
import scala.compiletime.*

class SemigroupKSuite extends KittensSuite {
  import SemigroupKSuite.*
  import TestDefns.*

  inline def semigroupKTests[F[_]]: SemigroupKTests[F] = SemigroupKTests[F](summonInline)

  inline def testSemigroupK(context: String): Unit = {
    checkAll(s"$context.SemigroupK[ComplexProduct]", semigroupKTests[ComplexProduct].semigroupK[Char])
    checkAll(s"$context.SemigroupK[CaseClassWOption]", semigroupKTests[CaseClassWOption].semigroupK[Char])
    checkAll(s"$context.SemigroupK[BoxMul]", semigroupKTests[BoxMul].semigroupK[Char])
    checkAll(
      s"$context.SemigroupK is Serializable",
      SerializableTests.serializable(summonInline[SemigroupK[ComplexProduct]])
    )

    test(s"$context.SemigroupK respects existing instances") {
      assert(summonInline[SemigroupK[BoxMul]].combineK(Box(Mul[Char](5)), Box(Mul[Char](5))) == Box(Mul[Char](25)))
    }
  }

  locally {
    import auto.semigroupK.given
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

  case class Simple[A](value1: List[A], value2: Set[A]) derives SemigroupK
  case class Recursive[A](first: List[A], rest: Recursive[A]) derives SemigroupK
}
