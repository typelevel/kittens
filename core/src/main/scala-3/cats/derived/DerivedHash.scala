package cats.derived

import cats.Hash
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

  // These instances support singleton types unlike the instances in Cats' kernel.
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

  trait Product[F[x] <: Hash[x], A <: scala.Product](using inst: ProductInstances[F, A])
      extends DerivedEq.Product[F, A],
        Hash[A]:

    final override def hash(x: A): Int =
      val arity = x.productArity
      val prefix = x.productPrefix.hashCode
      if arity <= 0 then prefix
      else
        val hash = inst.foldLeft[Int](x)(MurmurHash3.mix(MurmurHash3.productSeed, prefix)):
          [t] => (acc: Int, h: F[t], x: t) => MurmurHash3.mix(acc, h.hash(x))
        MurmurHash3.finalizeHash(hash, arity)

  trait Coproduct[F[x] <: Hash[x], A](using inst: CoproductInstances[F, A]) extends DerivedEq.Coproduct[F, A], Hash[A]:
    final override def hash(x: A): Int = inst.fold[Int](x)([t] => (h: F[t], x: t) => h.hash(x))

  object Strict:
    export DerivedHash.coproduct
    given product[A <: scala.Product: ProductInstancesOf[Hash]]: DerivedHash[A] =
      new Product[Hash, A] {}
