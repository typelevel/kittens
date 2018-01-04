package cats

import shapeless.Lazy


package object derived {

  @deprecated("use cats.derive.empty instead", "1.0.0-RC1")
  object empty extends MkEmptyDerivation

  object emptyK extends MkEmptyKDerivation

  @deprecated("use cats.derive.eq instead", "1.0.0-RC1")
  object eq extends MkEqDerivation

  object foldable extends MkFoldableDerivation

  object functor {
    implicit def derivedFunctor[F[_]](
      implicit ev: Refute[Functor[F]], F: Lazy[MkFunctor.LowPriority[F]]
    ): Functor[F] = F.value.instance
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
    implicit def derivedShow[A](
      implicit ev: Refute[Show[A]], A: Lazy[MkShow.LowPriority[A]]
    ): Show[A] = A.value.instance
  }
}
