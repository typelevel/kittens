package cats.derived

import cats.{Monoid, MonoidK}
import shapeless3.deriving.K1

object monoidk extends MonoidKDerivation

trait ProductMonoidK[T[x[_]] <: MonoidK[x], F[_]](using inst: K1.ProductInstances[T, F])
  extends ProductSemigroupK[T, F], MonoidK[F]:
    def empty[A]: F[A] = inst.construct([t[_]] => (emp: T[t]) => emp.empty[A])

trait MonoidKDerivation:
  extension (F: MonoidK.type)
    inline def derived[F[_]](using gen: K1.Generic[F]): MonoidK[F] = ???

  given [X](using X: Monoid[X]): MonoidK[Const[X]] with
    def empty[A]: Const[X][A] = X.empty

    def combineK[A](x: Const[X][A], y: Const[X][A]): Const[X][A] =
      X.combine(x, y)
