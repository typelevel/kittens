package cats
package derived

import scala.annotation.{implicitNotFound, tailrec}
import scala.util.hashing.MurmurHash3

@implicitNotFound("Could not derive an instance of Hash[${A}]")
trait MkHash[A] extends Hash[A]

object MkHash extends MkHashDerivation {
  def apply[A](implicit ev: MkHash[A]): MkHash[A] = ev
}

private[derived] trait HashBuilder[A] {
  def hashes(x: A): List[Int]
  def eqv(x: A, y: A): Boolean

  def hash(x: A, seed: Int): Int = {
    @tailrec def loop(hashes: List[Int], hash: Int, length: Int): Int = hashes match {
      case head :: tail => loop(tail, MurmurHash3.mix(hash, head), length + 1)
      case Nil => MurmurHash3.finalizeHash(hash, length)
    }

    loop(hashes(x), seed, 0)
  }
}

import shapeless._
import util.VersionSpecific.{OrElse, Lazy}

private[derived] object HashBuilder {

  implicit val hashBuilderHNil: HashBuilder[HNil] =
    instance(_ => Nil, (_, _) => true)

  implicit def hashBuilderHCons[H, T <: HList](
    implicit H: Hash[H] OrElse MkHash[H], T: HashBuilder[T]
  ): HashBuilder[H :: T] = instance(
    { case h :: t => H.unify.hash(h) :: T.hashes(t) },
    { case (hx :: tx, hy :: ty) => H.unify.eqv(hx, hy) && T.eqv(tx, ty) }
  )

  private def instance[A](f: A => List[Int], g: (A, A) => Boolean): HashBuilder[A] =
    new HashBuilder[A] {
      def hashes(x: A) = f(x)
      def eqv(x: A, y: A) = g(x, y)
    }
}

private[derived] abstract class MkHashDerivation extends MkHashGenericProduct {

  implicit val mkHashCNil: MkHash[CNil] =
    instance(_ => 0, (_, _) => true)

  implicit def mkHashCCons[L, R <: Coproduct](
    implicit L: Hash[L] OrElse MkHash[L], R: MkHash[R]
  ): MkHash[L :+: R] = instance({
    case Inl(l) => L.unify.hash(l)
    case Inr(r) => R.hash(r)
  }, {
    case (Inl(lx), Inl(ly)) => L.unify.eqv(lx, ly)
    case (Inr(rx), Inr(ry)) => R.eqv(rx, ry)
    case _ => false
  })

  implicit def mkHashCaseObject[A](implicit A: Generic.Aux[A, HNil]): MkHash[A] =
    instance(_.hashCode, (_, _) => true)
}

private[derived] abstract class MkHashGenericProduct extends MkHashGenericHList {

  implicit def mkHashGenericProduct[A, R <: HList](
    implicit A: Generic.Aux[A, R], R: Lazy[HashBuilder[R]], ev: A <:< Product
  ): MkHash[A] = instance(
    x => R.value.hash(A.to(x), util.VersionSpecific.productSeed(x)),
    (x, y) => R.value.eqv(A.to(x), A.to(y))
  )
}

private[derived] abstract class MkHashGenericHList extends MkHashGenericCoproduct {

  implicit def mkHashGenericHList[A, R <: HList](
    implicit A: Generic.Aux[A, R], R: Lazy[HashBuilder[R]]
  ): MkHash[A] = instance(
    x => R.value.hash(A.to(x), MurmurHash3.productSeed),
    (x, y) => R.value.eqv(A.to(x), A.to(y))
  )
}

private[derived] abstract class MkHashGenericCoproduct {

  implicit def mkHashGenericCoproduct[A, R <: Coproduct](
    implicit A: Generic.Aux[A, R], R: Lazy[MkHash[R]]
  ): MkHash[A] = instance(
    x => R.value.hash(A.to(x)),
    (x, y) => R.value.eqv(A.to(x), A.to(y))
  )

  protected def instance[A](f: A => Int, g: (A, A) => Boolean): MkHash[A] =
    new MkHash[A] {
      def hash(x: A) = f(x)
      def eqv(x: A, y: A) = g(x, y)
    }
}
