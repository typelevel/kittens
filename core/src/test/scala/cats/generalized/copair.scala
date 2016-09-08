package cats.generalized

import cats._
import cats.data._
import cats.laws._
import cats.implicits._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.scalatest.Matchers._
import org.scalatest.prop.Checkers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import cats.derived._

class CopairTest
  extends KittensSuite
  with GeneratorDrivenPropertyChecks
  with CopairSyntax
  with CopairInstances {

  CopairTests[Xor].copair[Option, Int, Int, Int, String, String, String].all.check
  CopairTests[Either].copair[Option, Int, Int, Int, String, String, String].all.check
  CopairTests[Validated].copair[Option, Int, Int, Int, String, String, String].all.check

  testCopairs[Xor]("Xor")
  testCopairs[Either]("Either")
  testCopairs[Validated]("Validated")

  def testCopairs[F[_,_]: Copair](ofType: String)(implicit arb: Arbitrary[F[String, Int]]): Unit = {
    test(s"$ofType Copair for-each performs side-effect") {

      forAll { copair: F[String, Int] =>
        var sideEffectOccurred = false
        copair.foreach(_ => sideEffectOccurred = true)

        sideEffectOccurred should === (copair.isRight)
      }
    }

    test(s"$ofType Copair for-all") {
      forAll { copair: F[String, Int] =>
        copair.forall(_ % 2 == 0) should === (copair.fold(_ => true, _ % 2 == 0))
      }
    }

    test(s"$ofType Copair exists") {
      forAll { copair: F[String, Int] =>
        copair.exists(_ % 2 == 0) should === (copair.fold(_ => false, _ % 2 == 0))
      }
    }

    test(s"$ofType Copair left/right") {
      forAll { copair: F[String, Int] =>
        copair.isLeft should === (copair.fold(_ => true, _ => false))
        copair.isRight should === (copair.fold(_ => false, _ => true))
      }
    }

    test(s"$ofType Copair to") {
      forAll { copair: F[String, Int] =>
        copair.to[Xor].isLeft should === (copair.isLeft)
        copair.to[Xor].isRight should === (copair.isRight)

        val (strFold, intFold): (String => String, Int => String) = (_ => "string", _ => "int")
        copair.to[Xor].fold(strFold, intFold) should === (copair.fold(strFold, intFold))
      }
    }

  }
}


trait CopairTests[F[_,_]] extends BitraverseTests[F] with BifoldableTests[F] with BifunctorTests[F] {
  def laws: CopairLaws[F]

  def copair[G[_], A, B, C, D, E, H](implicit
    G: Applicative[G],
    C: Monoid[C],
    ArbFAB: Arbitrary[F[A, B]],
    ArbFAD: Arbitrary[F[A, D]],
    ArbGC: Arbitrary[G[C]],
    ArbGD: Arbitrary[G[D]],
    ArbGE: Arbitrary[G[E]],
    ArbGH: Arbitrary[G[H]],
    ArbA: Arbitrary[A],
    ArbB: Arbitrary[B],
    ArbC: Arbitrary[C],
    ArbE: Arbitrary[E],
    ArbH: Arbitrary[H],
    EqFAB: Eq[F[A, B]],
    EqFAD: Eq[F[A, D]],
    EqFAH: Eq[F[A, H]],
    EqFCD: Eq[F[C, D]],
    EqFCH: Eq[F[C, H]],
    EqGGFEH: Eq[G[G[F[E, H]]]],
    EqC: Eq[C]
  ): RuleSet =
    new RuleSet {
      val name = "copair"
      val parents = Seq(bitraverse[G,A,B,C,D,E,H], bifoldable[A, B, C], bifunctor[A, B, C, D, E, H])
      val bases = Seq.empty
      val props = Seq(
        "copair fold identity" -> forAll(laws.copairFoldIdentity[A, B] _),
        "copair double swap identity" -> forAll(laws.copairDoubleSwapIdentity[A,B] _),
        "copair left swap identity" -> forAll(laws.copairLeftSwapIdentity[A, B] _),
        "copair right swap identity" -> forAll(laws.copairRightSwapIdentity[A, B] _),
        "copair left identity" -> forAll(laws.copairLeftIdentity[A,B,C] _),
        "copair right identity" -> forAll(laws.copairRightIdentity[A,B,C] _),
        "copair to identity" -> forAll(laws.copairToIdentity[A,B] _)
      )
    }
}

object CopairTests {
  def apply[F[_, _]: Copair]: CopairTests[F] =
    new CopairTests[F] { def laws: CopairLaws[F] = CopairLaws[F] }
}

trait CopairLaws[F[_,_]] extends BitraverseLaws[F] with BifoldableLaws[F] with BifunctorLaws[F] with CopairSyntax {
  implicit override def F: Copair[F]

  def copairFoldIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    fab <-> fab.fold(_.leftC[F, B], _.rightC[F, A])

  def copairDoubleSwapIdentity[A,B](fab: F[A,B]): IsEq[F[A,B]] =
    fab <-> fab.swap.swap

  def copairLeftIdentity[A, B, C](a: A, fa: A => C, fb: B => C): IsEq[C] =
    a.leftC[F, B].fold(fa, fb) <-> fa(a)

  def copairRightIdentity[A, B, C](b: B, fa: A => C, fb: B => C): IsEq[C] =
    b.rightC[F, A].fold(fa, fb) <-> fb(b)

  def copairToIdentity[A, B](fab: F[A,B]): IsEq[F[A,B]] =
    fab.to[F] <-> fab

  def copairLeftSwapIdentity[A, B](b: B): IsEq[F[A, B]] =
    b.leftC[F, A].swap <-> b.rightC[F, A]

  def copairRightSwapIdentity[A, B](a: A): IsEq[F[A, B]] =
    a.rightC[F, B].swap <-> a.leftC[F, B]

}

object CopairLaws {
  def apply[F[_, _]](implicit ev: Copair[F]): CopairLaws[F] =
    new CopairLaws[F] { def F: Copair[F] = ev }
}


