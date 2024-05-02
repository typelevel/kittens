package cats
package derived

import alleycats._
import cats.derived.util.VersionSpecific.Lazy
import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}
import shapeless.{Cached, Refute}

/** Fully automatically derive the instance, note that this derivation is not cached, so it will re-derive every time an
  * instance is needed in the application, which could significantly impact the compilation time.
  */
object auto {

  object iterable {
    implicit def kittensMkIterable[F[_], A](fa: F[A])(implicit F: MkIterable[F]): Iterable[A] =
      F.iterable(fa)
  }

  object empty {
    implicit def kittensMkEmpty[A](implicit
        refute: Refute[Empty[A]],
        ev: Lazy[MkEmpty[A]]
    ): Empty[A] = ev.value
  }

  object emptyK {
    implicit def kittensMkEmptyK[F[_]](implicit
        refute: Refute[EmptyK[F]],
        F: MkEmptyK[F]
    ): EmptyK[F] = F
  }

  object eq {
    implicit def kittensMkEq[A](implicit
        refute: Refute[Eq[A]],
        ev: Lazy[MkEq[A]]
    ): Eq[A] = ev.value
  }

  object partialOrder {
    implicit def kittensMkPartialOrder[A](implicit
        refute: Refute[PartialOrder[A]],
        ev: Lazy[MkPartialOrder[A]]
    ): PartialOrder[A] = ev.value
  }

  object order {
    implicit def kittensMkOrder[A](implicit
        refute: Refute[Order[A]],
        ev: Lazy[MkOrder[A]]
    ): Order[A] = ev.value
  }

  object hash {
    implicit def kittensMkHash[A](implicit
        refute: Refute[Hash[A]],
        ev: Lazy[MkHash[A]]
    ): Hash[A] = ev.value
  }

  object invariant {
    implicit def kittensMkInvariant[F[_]](implicit
        refute: Refute[Invariant[F]],
        F: MkInvariant[F]
    ): Invariant[F] = F
  }

  object contravariant {
    implicit def kittensMkContravariant[F[_]](implicit
        refute: Refute[Contravariant[F]],
        F: MkContravariant[F]
    ): Contravariant[F] = F
  }

  object functor {
    implicit def kittensMkFunctor[F[_]](implicit
        refute: Refute[Functor[F]],
        F: MkFunctor[F]
    ): Functor[F] = F
  }

  object apply {
    implicit def kittensMkApply[F[_]](implicit
        refute: Refute[Apply[F]],
        F: MkApply[F]
    ): Apply[F] = F
  }

  object applicative {
    implicit def kittensMkApplicative[F[_]](implicit
        refute: Refute[Applicative[F]],
        F: MkApplicative[F]
    ): Applicative[F] = F
  }

  object show {
    implicit def kittensMkShow[A](implicit
        refute: Refute[Show[A]],
        ev: Lazy[MkShow[A]]
    ): Show[A] = ev.value
  }

  object showPretty {
    implicit def kittensMkShowPretty[A](implicit
        refute: Refute[Show[A]],
        ev: Lazy[MkShowPretty[A]]
    ): ShowPretty[A] = ev.value
  }

  object semigroup {
    implicit def kittensMkSemigroup[A](implicit
        refute: Refute[Semigroup[A]],
        ev: Lazy[MkSemigroup[A]]
    ): Semigroup[A] = ev.value
  }

  object commutativeSemigroup {
    implicit def kittensMkCommutativeSemigroup[A](implicit
        refute: Refute[CommutativeSemigroup[A]],
        ev: Lazy[MkCommutativeSemigroup[A]]
    ): CommutativeSemigroup[A] = ev.value
  }

  object monoid {
    implicit def kittensMkMonoid[A](implicit
        refute: Refute[Monoid[A]],
        ev: Lazy[MkMonoid[A]]
    ): Monoid[A] = ev.value
  }

  object commutativeMonoid {
    implicit def kittensMkCommutativeMonoid[A](implicit
        refute: Refute[CommutativeMonoid[A]],
        ev: Lazy[MkCommutativeMonoid[A]]
    ): CommutativeMonoid[A] = ev.value
  }

  object semigroupK {
    implicit def kittensMkSemigroupK[F[_]](implicit
        refute: Refute[SemigroupK[F]],
        F: MkSemigroupK[F]
    ): SemigroupK[F] = F
  }

  object monoidK {
    implicit def kittensMkMonoidK[F[_]](implicit
        refute: Refute[MonoidK[F]],
        F: MkMonoidK[F]
    ): MonoidK[F] = F
  }

  object foldable {
    implicit def kittensMkFoldable[F[_]](implicit
        refute: Refute[Foldable[F]],
        F: MkFoldable[F]
    ): Foldable[F] = F
  }

  object reducible {
    implicit def kittensMkReducible[F[_]](implicit
        refute: Refute[Reducible[F]],
        F: MkReducible[F]
    ): Reducible[F] = F
  }

  object traverse {
    implicit def kittensMkTraverse[F[_]](implicit
        refute: Refute[Traverse[F]],
        F: MkTraverse[F]
    ): Traverse[F] = F
  }

  object nonEmptyTraverse {
    implicit def kittensMkNonEmptyTraverse[F[_]](implicit
        refute: Refute[NonEmptyTraverse[F]],
        F: MkNonEmptyTraverse[F]
    ): NonEmptyTraverse[F] = F
  }

  object pure {
    implicit def kittensMkPure[F[_]](implicit
        refute: Refute[Pure[F]],
        F: MkPure[F]
    ): Pure[F] = F
  }

  object consK {
    implicit def kittensMkConsK[F[_]](implicit
        refute: Refute[ConsK[F]],
        F: MkConsK[F, F]
    ): ConsK[F] = MkConsK.consK(F)
  }
}

/** cached cache the derived instance but this cache are global, so be cautious only use it when there is only one
  * instance globally in your application.
  */
object cached {

  object empty {
    implicit def kittensMkEmpty[A](implicit
        refute: Refute[Empty[A]],
        cached: Cached[MkEmpty[A]]
    ): Empty[A] = cached.value
  }

  object emptyK {
    implicit def kittensMkEmptyK[F[_]](implicit
        refute: Refute[EmptyK[F]],
        cached: Cached[MkEmptyK[F]]
    ): EmptyK[F] = cached.value
  }

  object pure {
    implicit def kittensMkPure[F[_]](implicit
        refute: Refute[Pure[F]],
        cached: Cached[MkPure[F]]
    ): Pure[F] = cached.value
  }

  object eq {
    implicit def kittensMkEq[A](implicit
        refute: Refute[Eq[A]],
        cached: Cached[MkEq[A]]
    ): Eq[A] = cached.value
  }

  object partialOrder {
    implicit def kittensMkPartialOrder[A](implicit
        refute: Refute[PartialOrder[A]],
        cached: Cached[MkPartialOrder[A]]
    ): PartialOrder[A] = cached.value
  }

  object order {
    implicit def kittensMkOrder[A](implicit
        refute: Refute[Order[A]],
        cached: Cached[MkOrder[A]]
    ): Order[A] = cached.value
  }

  object hash {
    implicit def kittensMkHash[A](implicit
        refute: Refute[Hash[A]],
        cached: Cached[MkHash[A]]
    ): Hash[A] = cached.value
  }

  object contravariant {
    implicit def kittensMkContravariant[F[_]](implicit
        refute: Refute[Contravariant[F]],
        cached: Cached[MkContravariant[F]]
    ): Contravariant[F] = cached.value
  }

  object functor {
    implicit def kittensMkFunctor[F[_]](implicit
        refute: Refute[Functor[F]],
        cached: Cached[MkFunctor[F]]
    ): Functor[F] = cached.value
  }

  object apply {
    implicit def kittensMkApply[F[_]](implicit
        refute: Refute[Apply[F]],
        cached: Cached[MkApply[F]]
    ): Apply[F] = cached.value
  }

  object applicative {
    implicit def kittensMkApplicative[F[_]](implicit
        refute: Refute[Applicative[F]],
        cached: Cached[MkApplicative[F]]
    ): Applicative[F] = cached.value
  }

  object foldable {
    implicit def kittensMkFoldable[F[_]](implicit
        refute: Refute[Foldable[F]],
        cached: Cached[MkFoldable[F]]
    ): Foldable[F] = cached.value
  }

  object reducible {
    implicit def kittensMkReducible[F[_]](implicit
        refute: Refute[Reducible[F]],
        cached: Cached[MkReducible[F]]
    ): Reducible[F] = cached.value
  }

  object invariant {
    implicit def kittensMkInvariant[F[_]](implicit
        refute: Refute[Invariant[F]],
        cached: Cached[MkInvariant[F]]
    ): Invariant[F] = cached.value
  }

  object traverse {
    implicit def kittensMkTraverse[F[_]](implicit
        refute: Refute[Traverse[F]],
        cached: Cached[MkTraverse[F]]
    ): Traverse[F] = cached.value
  }

  object nonEmptyTraverse {
    implicit def kittensMkNonEmptyTraverse[F[_]](implicit
        refute: Refute[NonEmptyTraverse[F]],
        cached: Cached[MkNonEmptyTraverse[F]]
    ): NonEmptyTraverse[F] = cached.value
  }

  object show {
    implicit def kittensMkshow[A](implicit
        refute: Refute[Show[A]],
        cached: Cached[MkShow[A]]
    ): Show[A] = cached.value
  }

  object showPretty {
    implicit def kittensMkShowPretty[A](implicit
        refute: Refute[Show[A]],
        cached: Cached[MkShowPretty[A]]
    ): ShowPretty[A] = cached.value
  }

  object monoidK {
    implicit def kittensMkMonoidK[F[_]](implicit
        refute: Refute[MonoidK[F]],
        cached: Cached[MkMonoidK[F]]
    ): MonoidK[F] = cached.value
  }

  object semigroup {
    implicit def kittensMkSemigroup[A](implicit
        refute: Refute[Semigroup[A]],
        cached: Cached[MkSemigroup[A]]
    ): Semigroup[A] = cached.value
  }

  object commutativeSemigroup {
    implicit def kittensMkCommutativeSemigroup[A](implicit
        refute: Refute[CommutativeSemigroup[A]],
        cached: Cached[MkCommutativeSemigroup[A]]
    ): CommutativeSemigroup[A] = cached.value
  }

  object monoid {
    implicit def kittensMkMonoid[A](implicit
        refute: Refute[Monoid[A]],
        cached: Cached[MkMonoid[A]]
    ): Monoid[A] = cached.value
  }

  object commutativeMonoid {
    implicit def kittensMkCommutativeMonoid[A](implicit
        refute: Refute[CommutativeMonoid[A]],
        cached: Cached[MkCommutativeMonoid[A]]
    ): CommutativeMonoid[A] = cached.value
  }

  object semigroupK {
    implicit def kittensMkSemigroupK[F[_]](implicit
        refute: Refute[SemigroupK[F]],
        cached: Cached[MkSemigroupK[F]]
    ): SemigroupK[F] = cached.value
  }

  object consK {
    implicit def kittensMkConsK[F[_]](implicit
        refute: Refute[ConsK[F]],
        cached: Cached[MkConsK[F, F]]
    ): ConsK[F] = MkConsK.consK(cached.value)
  }
}
