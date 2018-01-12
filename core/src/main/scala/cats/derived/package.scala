package cats

import alleycats._
import shapeless.Refute

/** Full auto derivation of type classes. */
package object derived {

  object empty {
    implicit def mkEmpty[A](
      implicit refute: Refute[Empty[A]], empty: MkEmpty[A]
    ): Empty[A] = empty
  }

  object emptyK extends MkEmptyKDerivation

  @deprecated("use cats.derive.eq instead", "1.0.0-RC1")
  object eq extends MkEqDerivation

  object foldable extends MkFoldableDerivation

  object functor {
    implicit def mkFunctor[F[_]](
      implicit refute: Refute[Functor[F]], functor: MkFunctor[F]
    ): Functor[F] = functor
  }

  object iterable extends IterableDerivationFromMkIterable

  @deprecated("use cats.derive.monoid instead", "1.0.0-RC1")
  object monoid extends MkMonoidDerivation

  @deprecated("use cats.derive.monoidK instead", "1.0.0-RC1")
  object monoidK extends MkMonoidK0

  object pure extends MkPureDerivation

  @deprecated("use cats.derive.semigroup instead", "1.0.0-RC1")
  object semigroup extends MkSemigroupDerivation

  @deprecated("use cats.derive.semigroupK instead", "1.0.0-RC1")
  object semigroupK extends MkSemigroupK0

  @deprecated("use cats.derive.show instead", "1.0.0-RC1")
  object show extends MkShowDerivation
}
