package cats
package derived

import shapeless._
import util.VersionSpecific.OrElse

import scala.annotation.implicitNotFound

@implicitNotFound("Could not derive an instance of Contravariant[${F}]")
trait MkContravariant[F[_]] extends Contravariant[F] {
  def safeContramap[A, B](fa: F[A])(f: B => Eval[A]): Eval[F[B]]
  def contramap[A, B](fa: F[A])(f: B => A): F[B] = safeContramap(fa)(f andThen Eval.now).value
}
object MkContravariantvariant extends MkContravariantvariantDerivation {
  def apply[F[_]](implicit F: MkContravariant[F]): MkContravariant[F] = F
}
private[derived] abstract class MkContravariantvariantDerivation extends MkContravariantvariantNested {
  implicit val mkContraHNil: MkContravariant[Const[HNil]#λ] = mkContraConst
  implicit val mkContraCNil: MkContravariant[Const[CNil]#λ] = mkContraConst

  implicit def mkContraConst[T]: MkContravariant[Const[T]#λ] =
    new MkContravariant[Const[T]#λ] {
      def safeContramap[A, B](t: T)(f: B => Eval[A]): Eval[T] = Eval.now(t)
    }
}

private[derived] abstract class MkContravariantvariantNested extends MkContravariantCons {
  implicit def mkContravariantFunctorNested[F[_]](implicit F: Split1[F, FunctorOrMk, ContraOrMk]): MkContravariant[F] =
    new MkContravariant[F] {
      def safeContramap[A, B](fa: F[A])(f: B => Eval[A]): Eval[F[B]] =
        mkSafeMap(F.fo)(F.unpack(fa))(
          (a: F.I[A]) => mkContraSafe(F.fi)(a)(f)
        ).map(F.pack)
    }

  implicit def mkContravariantThriceNested[F[_], I[_], J[_], K[_]](
    implicit
    F: Split1.Aux[F, ContraOrMk, ContraOrMk, I, J],
    G: Split1[J, ContraOrMk, ContraOrMk]
  ): MkContravariant[F] =
    new MkContravariant[F] {
      override def safeContramap[A, B](fa: F[A])(f: B => Eval[A]): Eval[F[B]] =
        mkContraSafe(F.fo)(F.unpack(fa))(
          (a: F.I[B]) =>
            mkContraSafe(G.fo)(G.unpack(a))(
              (b: G.I[A]) => mkContraSafe(G.fi)(b)(f)
            ).map(G.pack)
        ).map(F.pack)
    }
}

private[derived] abstract class MkContravariantCons extends MkContravariantGeneric {
  implicit def mkContraHCons[F[_]](implicit F: IsHCons1[F, ContraOrMk, MkContravariant]): MkContravariant[F] =
    new MkContravariant[F] {
      def safeContramap[A, B](fa: F[A])(f: B => Eval[A]): Eval[F[B]] =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
          for {
            fhb <- mkContraSafe(F.fh)(fha)(f)
            ftb <- F.ft.safeContramap(fta)(f)
          } yield F.pack(fhb, ftb)
        }
    }

  implicit def mkContraCCons[F[_]](implicit F: IsCCons1[F, ContraOrMk, MkContravariant]): MkContravariant[F] =
    new MkContravariant[F] {
      def safeContramap[A, B](fa: F[A])(f: B => Eval[A]): Eval[F[B]] = F.unpack(fa) match {
        case Left(fha) => mkContraSafe(F.fh)(fha)(f).map(fhb => F.pack(Left(fhb)))
        case Right(fta) => F.ft.safeContramap(fta)(f).map(ftb => F.pack(Right(ftb)))
      }
    }
}

private[derived] abstract class MkContravariantGeneric {
  protected type FunctorOrMk[F[_]] = Functor[F] OrElse MkFunctor[F]
  protected def mkSafeMap[F[_], A, B](F: FunctorOrMk[F])(fa: F[A])(f: A => Eval[B]): Eval[F[B]] =
    F.unify match {
      case mk: MkFunctor[F] => mk.safeMap(fa)(f)
      case p => Eval.later(p.map(fa)(f(_).value))
    }

  protected type ContraOrMk[F[_]] = Contravariant[F] OrElse MkContravariant[F]

  protected def mkContraSafe[F[_], A, B](F: ContraOrMk[F])(fa: F[B])(f: A => Eval[B]): Eval[F[A]] =
    F.unify match {
      case mk: MkContravariant[F] => mk.safeContramap(fa)(f)
      case p => Eval.later(p.contramap(fa)(f(_).value))
    }

  implicit def mkFunctorGeneric[F[_]](implicit F: Generic1[F, MkContravariant]): MkContravariant[F] =
    new MkContravariant[F] {
      def safeContramap[A, B](fa: F[A])(f: B => Eval[A]): Eval[F[B]] =
        F.fr.safeContramap(F.to(fa))(f).map(F.from[B])
    }
}

