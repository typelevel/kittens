package cats.derived

import cats.Hash
import shapeless3.deriving.{K0, Continue}

import scala.annotation.*
import scala.compiletime.*
import scala.util.hashing.MurmurHash3

@implicitNotFound("""Could not derive an instance of Hash[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have a Hash instance
  * it is a sealed trait where all subclasses have a Hash instance""")
type DerivedHash[A] = Derived[Hash[A]]
object DerivedHash:
  type Or[A] = Derived.Or[Hash[A]]

  @nowarn("msg=unused import")
  inline def apply[A]: Hash[A] =
    import DerivedHash.given
    summonInline[DerivedHash[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: Hash[A] =
    import DerivedHash.given
    import Strict.product
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

  given product[A <: scala.Product](using inst: => K0.ProductInstances[Or, A]): DerivedHash[A] =
    Strict.product(using inst.unify)

  given coproduct[A](using inst: => K0.CoproductInstances[Or, A]): DerivedHash[A] =
    given K0.CoproductInstances[Hash, A] = inst.unify
    new Coproduct[Hash, A] {}

  trait Product[F[x] <: Hash[x], A <: scala.Product](using inst: K0.ProductInstances[F, A])
      extends DerivedEq.Product[F, A],
        Hash[A]:

    final override def hash(x: A): Int =
      val arity = x.productArity
      val prefix = x.productPrefix.hashCode
      if arity <= 0 then prefix
      else
        val hash = inst.foldLeft[Int](x)(MurmurHash3.mix(MurmurHash3.productSeed, prefix)):
          [t] => (acc: Int, h: F[t], x: t) => Continue(MurmurHash3.mix(acc, h.hash(x)))
        MurmurHash3.finalizeHash(hash, arity)

  trait Coproduct[F[x] <: Hash[x], A](using inst: K0.CoproductInstances[F, A])
      extends DerivedEq.Coproduct[F, A],
        Hash[A]:

    final override def hash(x: A): Int =
      inst.fold[Int](x)([t] => (h: F[t], x: t) => h.hash(x))

  object Strict:
    given product[A <: scala.Product](using K0.ProductInstances[Hash, A]): DerivedHash[A] =
      new Product[Hash, A] {}
