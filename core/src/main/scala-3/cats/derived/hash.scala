package cats.derived

import cats.Hash
import shapeless3.deriving.{K0, Continue}

import scala.compiletime.summonInline
import scala.util.hashing.MurmurHash3

trait ProductHash[T[x] <: Hash[x], A](using inst: K0.ProductInstances[T, A], ev: A <:< Product)
  extends ProductEq[T, A], Hash[A]:

  def hash(x: A): Int = {
    val hash = inst.foldLeft[Int](x)((MurmurHash3.productSeed))(
      [t] => (acc: Int, h: T[t], t0: t) => Continue(MurmurHash3.mix(acc, h.hash(t0)))
    )
    MurmurHash3.finalizeHash(hash, ev(x).productArity)
  }

trait CoproductHash[T[x] <: Hash[x], A](using inst: K0.CoproductInstances[T, A])
  extends CoproductEq[T, A],  Hash[A]:

  def hash(x: A): Int = inst.fold[Int](x)(
    [t] => (h: T[t], t0: t) => h.hash(t0)
  )

trait HashDerivation:
  extension (F: Hash.type)
    inline def derived[A](using gen: K0.Generic[A]): Hash[A] =
      inline gen match {
        case p: K0.ProductGeneric[A]   => productHash(using p.asInstanceOf, summonInline[A <:< Product])
        case c: K0.CoproductGeneric[A] => coproductHash(using c.asInstanceOf)
      }

  given productHash[A](using K0.ProductInstances[Hash, A], A <:< Product): Hash[A] =
    new ProductHash[Hash, A]{}

  given coproductHash[A](using K0.CoproductInstances[Hash, A]): Hash[A] =
    new CoproductHash[Hash, A]{}
