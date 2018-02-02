package cats

import alleycats._
import shapeless.{Cached, Refute}

/** Full auto derivation of type classes. */
package object derived {

  object empty {
    implicit def kittensMkEmpty[A](
      implicit refute: Refute[Empty[A]], empty: Cached[MkEmpty[A]]
    ): Empty[A] = empty.value
  }

  object emptyK extends MkEmptyKDerivation

  object eq {
    implicit def kittensMkEq[A](
      implicit refute: Refute[Eq[A]], eq: Cached[MkEq[A]]
    ): Eq[A] = eq.value
  }

  object foldable extends MkFoldableDerivation

  object functor {
    implicit def kittensMkFunctor[F[_]](
      implicit refute: Refute[Functor[F]], functor: Cached[MkFunctor[F]]
    ): Functor[F] = functor.value
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

  object show {
    implicit def kittensMkShow[A](
      implicit refute: Refute[Show[A]], show: Cached[MkShow[A]]
    ): Show[A] = show.value
  }
}
