package cats.derived

import cats.Hash
import shapeless._, labelled._

trait MkHash[A] extends Hash[A]



object MkHash extends MkHash0 {
  def apply[A](implicit hash: MkHash[A]): MkHash[A] = hash


}

private[derived] trait HashBuilder[A] {
  def hashes(a: A): List[Int]
  def eqv(x: A, y: A): Boolean
}

object HashBuilder {
  implicit val emptyProductDerivedHash: HashBuilder[HNil] = new HashBuilder[HNil] {
    override def hashes(x: HNil): List[Int] = Nil
    override def eqv(x: HNil, y: HNil): Boolean = true
  }


  implicit def productDerivedHash[H, T <: HList](
                                                  implicit
                                                  hashH: Hash[H] OrElse MkHash[H],
                                                  hashT: HashBuilder[T]): HashBuilder[H :: T] = new HashBuilder[H :: T] {
    def hashes(fields: H :: T): List[Int] = {
      val h = hashH.unify.hash(fields.head)
      val t = hashT.hashes(fields.tail)
      h :: t
    }

    def eqv(x: H :: T, y: H :: T): Boolean =
      hashH.unify.eqv(x.head, y.head) && hashT.eqv(x.tail, y.tail)
  }
}

trait MkHash0 extends MkHash1 {
  implicit def deriveHashCaseObject[A](
                                      implicit repr: Generic.Aux[A, HNil]): MkHash[A] = new MkHash[A] {
    def hash(x: A): Int = x.hashCode

    def eqv(x: A, y: A): Boolean = true
  }

}

trait MkHash1 {
  implicit def fromBuilder[A](implicit builder: HashBuilder[A]): MkHash[A] = new MkHash[A] {
    override def hash(x: A): Int = {
      val hashes = builder.hashes(x)
      runtime.Statics.finalizeHash(hashes.foldLeft(-889275714)(runtime.Statics.mix), hashes.length)
    }

    override def eqv(x: A, y: A): Boolean = builder.eqv(x, y)
  }

  implicit def emptyCoproductDerivedHash: MkHash[CNil] = null
  // used when Hash[V] (a member of the coproduct) has to be derived.
  implicit def coproductDerivedHash[L, R <: Coproduct](
     implicit
     hashV: Hash[L] OrElse MkHash[L],
     hashT: MkHash[R]): MkHash[L :+: R] = new MkHash[L :+: R] {
    def hash(value: L :+: R): Int = value match {
      case Inl(l) => hashV.unify.hash(l)
      case Inr(r) => hashT.hash(r)
    }

    def eqv(x: L :+: R, y: L :+: R): Boolean =
      (x, y) match {
        case (Inl(xl), Inl(yl)) => hashV.unify.eqv(xl, yl)
        case (Inr(xr), Inr(yr)) => hashT.eqv(xr, yr)
        case _ => false
      }

  }

  implicit def genericDerivedHash[A, R](
                                         implicit gen: Generic.Aux[A, R],
                                         s: Lazy[MkHash[R]]): MkHash[A] = new MkHash[A] {
    def hash(a: A): Int = s.value.hash(gen.to(a))

    def eqv(x: A, y: A): Boolean = s.value.eqv(gen.to(x), gen.to(y))
  }
}

