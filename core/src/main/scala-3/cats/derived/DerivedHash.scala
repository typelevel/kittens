package cats.derived

import cats.{Eval, Hash}
import shapeless3.deriving.Derived
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*
import scala.util.hashing.MurmurHash3

@implicitNotFound("""Could not derive Hash for ${A}.
Make sure it satisfies one of the following conditions:
  * case class where all fields form Hash
  * sealed trait where all subclasses form Hash
  * enum where all variants form Hash""")
type DerivedHash[A] = Derived[Hash[A]]
object DerivedHash:
  inline def apply[A]: Hash[A] =
    import DerivedHash.given
    summonInline[DerivedHash[A]].instance

  inline def strict[A]: Hash[A] =
    import Strict.given
    summonInline[DerivedHash[A]].instance

  given boolean[A <: Boolean]: DerivedHash[A] = Hash.fromUniversalHashCode
  given byte[A <: Byte]: DerivedHash[A] = Hash.fromUniversalHashCode
  given short[A <: Short]: DerivedHash[A] = Hash.fromUniversalHashCode
  given int[A <: Int]: DerivedHash[A] = Hash.fromUniversalHashCode
  given long[A <: Long]: DerivedHash[A] = Hash.fromUniversalHashCode
  given float[A <: Float]: DerivedHash[A] = Hash.fromUniversalHashCode
  given double[A <: Double]: DerivedHash[A] = Hash.fromUniversalHashCode
  given char[A <: Char]: DerivedHash[A] = Hash.fromUniversalHashCode
  given string[A <: String]: DerivedHash[A] = Hash.fromUniversalHashCode
  given symbol[A <: Symbol]: DerivedHash[A] = Hash.fromUniversalHashCode

  given product[A <: scala.Product](using inst: => ProductInstances[Hash |: Derived, A]): DerivedHash[A] =
    Strict.product(using inst.unify)

  given coproduct[A](using inst: => CoproductInstances[Hash |: Derived, A]): DerivedHash[A] =
    given CoproductInstances[Hash, A] = inst.unify
    new Coproduct[Hash, A] {}

  private[derived] trait Safe[A] extends Hash[A]:
    private[derived] def safeHash(x: A): Eval[Int]
    override def hash(x: A): Int = safeHash(x).value

  private[derived] def safeHash[A](F: Hash[A])(x: A): Eval[Int] =
    F.asInstanceOf[Matchable] match
      case safe: Safe[?] => safe.asInstanceOf[Safe[A]].safeHash(x)
      case _ => Eval.later(F.hash(x))

  trait Product[F[x] <: Hash[x], A <: scala.Product](using inst: ProductInstances[F, A])
      extends DerivedEq.Product[F, A],
        Safe[A]:

    private[derived] final override def safeHash(x: A): Eval[Int] =
      val arity = x.productArity
      val prefix = x.productPrefix.hashCode
      if arity <= 0 then Eval.now(prefix)
      else
        val seed = MurmurHash3.mix(MurmurHash3.productSeed, prefix)
        inst.foldLeft[Eval[Int]](x)(Eval.now(seed)):
          [t] => (acc: Eval[Int], h: F[t], xt: t) =>
            acc.flatMap(a => DerivedHash.safeHash(h)(xt).map(hh => MurmurHash3.mix(a, hh)))
        .map(MurmurHash3.finalizeHash(_, arity))

  trait Coproduct[F[x] <: Hash[x], A](using inst: CoproductInstances[F, A]) extends DerivedEq.Coproduct[F, A], Safe[A]:
    private[derived] final override def safeHash(x: A): Eval[Int] =
      Eval.defer(inst.fold[Eval[Int]](x)([t] => (h: F[t], xt: t) => DerivedHash.safeHash(h)(xt)))

  object Strict:
    export DerivedHash.coproduct
    given product[A <: scala.Product: ProductInstancesOf[Hash]]: DerivedHash[A] =
      new Product[Hash, A] {}
