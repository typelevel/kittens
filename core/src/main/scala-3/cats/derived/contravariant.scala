package cats.derived

import cats.Contravariant
import shapeless3.deriving.{K1, Continue}

object contravariant extends ContravariantDerivation

trait GenericContravariant[T[x[_]] <: Contravariant[x], F[_]](using inst: K1.Instances[T, F])
  extends Contravariant[F]:

  def contramap[A, B](fa: F[A])(f: B => A): F[B] = inst.map(fa)(
    [t[_]] => (contra: T[t], t0: t[A]) => contra.contramap(t0)(f)
  )

trait ContravariantDerivation:
  extension (F: Contravariant.type)
    inline def derived[F[_]](using gen: K1.Generic[F]): Contravariant[F] =
      new GenericContravariant[Contravariant, F]{}

  given [X]: Contravariant[Const[X]] with
    def contramap[A, B](fa: X)(f: B => A): X = fa
