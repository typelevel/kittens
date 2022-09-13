package cats.derived

import cats.SemigroupK
import cats.laws.discipline.{SemigroupKTests, SerializableTests}
import scala.compiletime.*

class SemigroupKSuite extends KittensSuite:
  import SemigroupKSuite.*
  import TestDefns.*

  inline def tests[F[_]]: SemigroupKTests[F] =
    SemigroupKTests[F](summonInline)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance[ComplexProduct]", tests[ComplexProduct].semigroupK[Char])
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].semigroupK[Char])
    checkAll(s"$instance[BoxMul]", tests[BoxMul].semigroupK[Char])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[SemigroupK[ComplexProduct]]))
    test(s"$instance respects existing instances") {
      assert(summonInline[SemigroupK[BoxMul]].combineK(Box(Mul[Char](5)), Box(Mul[Char](5))) == Box(Mul[Char](25)))
    }

  locally {
    import auto.semigroupK.given
    validate("auto.semigroupK")
  }

  locally {
    import semiInstances.given
    validate("semiauto.semigroupK")
  }

  locally {
    import derivedInstances.*
    val instance = "derived.semigroupK"
    checkAll(s"$instance[ComplexProduct]", tests[ComplexProduct].semigroupK[Char])
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].semigroupK[Char])
    checkAll(s"$instance[Simple]", tests[Simple].semigroupK[Char])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[SemigroupK[ComplexProduct]]))
  }

end SemigroupKSuite

object SemigroupKSuite:
  import TestDefns.*

  type BoxMul[A] = Box[Mul[A]]

  object semiInstances:
    given SemigroupK[ComplexProduct] = semiauto.semigroupK
    given SemigroupK[CaseClassWOption] = semiauto.semigroupK
    given SemigroupK[BoxMul] = semiauto.semigroupK

  object derivedInstances:
    case class ComplexProduct[A](x: TestDefns.ComplexProduct[A]) derives SemigroupK
    case class CaseClassWOption[A](x: TestDefns.CaseClassWOption[A]) derives SemigroupK
    case class Simple[A](value1: List[A], value2: Set[A]) derives SemigroupK
    case class Recursive[A](first: List[A], rest: Recursive[A]) derives SemigroupK

  final case class Mul[T](value: Int)
  object Mul:
    given SemigroupK[Mul] with
      def combineK[A](x: Mul[A], y: Mul[A]) = Mul(x.value * y.value)

end SemigroupKSuite
