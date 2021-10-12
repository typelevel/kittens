package cats.derived

import cats.Hash
import shapeless3.deriving.{K0, Continue}

import scala.compiletime.*
import scala.util.hashing.MurmurHash3

type DerivedHash[A] = Derived[Hash[A]]
object DerivedHash:
  type Or[A] = Derived.Or[Hash[A]]
  inline def apply[A]: Hash[A] =
    import DerivedHash.given
    summonInline[DerivedHash[A]].instance

  given [A <: scala.Product](using inst: K0.ProductInstances[Or, A]): DerivedHash[A] =
    given K0.ProductInstances[Hash, A] = inst.unify
    new Product[Hash, A] {}

  given [A](using inst: => K0.CoproductInstances[Or, A]): DerivedHash[A] =
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
        MurmurHash3.finalizeHash(
          inst.foldLeft[Int](x)(MurmurHash3.mix(MurmurHash3.productSeed, prefix))(
            [t] => (acc: Int, h: F[t], x: t) => Continue(MurmurHash3.mix(acc, h.hash(x)))
          ),
          arity
        )

  trait Coproduct[F[x] <: Hash[x], A](using inst: K0.CoproductInstances[F, A])
      extends DerivedEq.Coproduct[F, A],
        Hash[A]:

    final override def hash(x: A): Int =
      inst.fold[Int](x)([t] => (h: F[t], x: t) => h.hash(x))
