package cats.derived

import cats.Hash
import shapeless3.deriving.{K0, Continue}

import scala.util.hashing.MurmurHash3

object hash extends HashDerivation

trait ProductHash[T[x] <: Hash[x], A](using inst: K0.ProductInstances[T, A])
  extends ProductEq[T, A], Hash[A]:

  def hash(x: A): Int = {
    val (hash, len) = inst.foldLeft[(Int, Int)](x)((MurmurHash3.productSeed, 0))(
      [t] => (acc: (Int, Int), h: T[t], t0: t) => Continue((MurmurHash3.mix(acc._1, h.hash(t0)), acc._2 + 1))
    )
    MurmurHash3.finalizeHash(hash, len)
  }

trait CoproductHash[T[x] <: Hash[x], A](using inst: K0.CoproductInstances[T, A])
  extends CoproductEq[T, A],  Hash[A]:

  def hash(x: A): Int = inst.fold[Int](x)(
    [t] => (h: T[t], t0: t) => h.hash(t0)
  )

trait HashDerivation:
  extension (F: Hash.type)
    inline def derived[A](using gen: K0.Generic[A]): Hash[A] =
      gen.derive(productHash, coproductHash)

  given productHash[A](using K0.ProductInstances[Hash, A]): Hash[A] =
    new ProductHash[Hash, A]{}

  given coproductHash[A](using K0.CoproductInstances[Hash, A]): Hash[A] =
    new CoproductHash[Hash, A]{}
