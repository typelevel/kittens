package cats.derived

import cats.{Applicative, Eval, Monoid, MonoidK}
import cats.laws.discipline.{MonoidKTests, SerializableTests}
import shapeless3.deriving.Const

import scala.compiletime.*

class MonoidKSuite extends KittensSuite:
  import MonoidKSuite.*
  import ADTs.*

  inline def tests[F[_]]: MonoidKTests[F] =
    MonoidKTests[F](using summonInline)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance[ComplexProduct]", tests[ComplexProduct].monoidK[Char])
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].monoidK[Char])
    checkAll(s"$instance[BoxMul]", tests[BoxMul].monoidK[Char])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[MonoidK[ComplexProduct]]))
    test(s"$instance respects existing instances"):
      val M = summonInline[MonoidK[BoxMul]]
      assert(M.empty[Char] == Box(Mul[Char](1)))
      assert(M.combineK(Box(Mul[Char](5)), Box(Mul[Char](5))) == Box(Mul[Char](25)))

  locally:
    import auto.monoidK.given
    validate("auto.monoidK")

  locally:
    import semiInstances.given
    validate("semiauto.monoidK")

  locally:
    import strictInstances.given
    validate("strict.semiauto.monoidK")
    testNoInstance("strict.semiauto.monoidK", "TopK")

  locally:
    import derivedInstances.*
    val instance = "derived.monoidK"
    checkAll(s"$instance[ComplexProduct]", tests[ComplexProduct].monoidK[Char])
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].monoidK[Char])
    checkAll(s"$instance[Simple]", tests[Simple].monoidK[Char])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(MonoidK[ComplexProduct]))

end MonoidKSuite

object MonoidKSuite:
  import ADTs.*

  type BoxMul[A] = Box[Mul[A]]

  object semiInstances:
    given MonoidK[ComplexProduct] = semiauto.monoidK
    given MonoidK[CaseClassWOption] = semiauto.monoidK
    given MonoidK[BoxMul] = semiauto.monoidK

  object strictInstances:
    given [T: Monoid]: MonoidK[Const[T]] = semiauto.monoidK
    given [F[_]: MonoidK, G[_]]: MonoidK[[x] =>> F[G[x]]] = MonoidK[F].compose[G]
    given [F[_]: Applicative, G[_]: MonoidK]: MonoidK[[x] =>> F[G[x]]] = semiauto.monoidK
    given MonoidK[ComplexProduct] = strict.semiauto.monoidK
    given MonoidK[CaseClassWOption] = strict.semiauto.monoidK
    given MonoidK[BoxMul] = strict.semiauto.monoidK

  object derivedInstances:
    case class ComplexProduct[A](x: ADTs.ComplexProduct[A]) derives MonoidK
    case class CaseClassWOption[A](x: ADTs.CaseClassWOption[A]) derives MonoidK
    case class Simple[A](value1: List[A], value2: Set[A]) derives MonoidK
    case class Recursive[A](first: List[A], rest: Recursive[A]) derives MonoidK

  final case class Mul[T](value: Int)
  object Mul:
    given MonoidK[Mul] with
      def empty[A] = Mul(1)
      def combineK[A](x: Mul[A], y: Mul[A]) = Mul(x.value * y.value)

end MonoidKSuite
