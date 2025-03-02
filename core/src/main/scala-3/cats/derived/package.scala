package cats.derived

import alleycats.*
import cats.*
import cats.kernel.{Band, BoundedSemilattice, CommutativeGroup, CommutativeMonoid, CommutativeSemigroup, Semilattice}

import scala.util.NotGiven

extension (x: Eq.type) inline def derived[A]: Eq[A] = DerivedEq[A]
extension (x: Hash.type) inline def derived[A]: Hash[A] = DerivedHash[A]
extension (x: Empty.type) inline def derived[A]: Empty[A] = DerivedEmpty[A]
extension (x: Semigroup.type) inline def derived[A]: Semigroup[A] = DerivedSemigroup[A]
extension (x: Monoid.type) inline def derived[A]: Monoid[A] = DerivedMonoid[A]
extension (x: Group.type) inline def derived[A]: Group[A] = DerivedGroup[A]
extension (x: Band.type) inline def derived[A]: Band[A] = DerivedBand[A]
extension (x: Order.type) inline def derived[A]: Order[A] = DerivedOrder[A]
extension (x: CommutativeSemigroup.type) inline def derived[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup[A]
extension (x: CommutativeMonoid.type) inline def derived[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid[A]
extension (x: CommutativeGroup.type) inline def derived[A]: CommutativeGroup[A] = DerivedCommutativeGroup[A]
extension (x: Semilattice.type) inline def derived[A]: Semilattice[A] = DerivedSemilattice[A]
extension (x: BoundedSemilattice.type) inline def derived[A]: BoundedSemilattice[A] = DerivedBoundedSemilattice[A]
extension (x: Show.type) inline def derived[A]: Show[A] = DerivedShow[A]
extension (x: Applicative.type) inline def derived[F[_]]: Applicative[F] = DerivedApplicative[F]
extension (x: Apply.type) inline def derived[F[_]]: Apply[F] = DerivedApply[F]
extension (x: NonEmptyAlternative.type) inline def derived[F[_]]: NonEmptyAlternative[F] = DerivedNonEmptyAlternative[F]
extension (x: Alternative.type) inline def derived[F[_]]: Alternative[F] = DerivedAlternative[F]
extension (x: EmptyK.type) inline def derived[F[_]]: EmptyK[F] = DerivedEmptyK[F]
extension (x: Pure.type) inline def derived[F[_]]: Pure[F] = DerivedPure[F]
extension (x: Foldable.type) inline def derived[F[_]]: Foldable[F] = DerivedFoldable[F]
extension (x: Functor.type) inline def derived[F[_]]: Functor[F] = DerivedFunctor[F]
extension (x: Reducible.type) inline def derived[F[_]]: Reducible[F] = DerivedReducible[F]
extension (x: Traverse.type) inline def derived[F[_]]: Traverse[F] = DerivedTraverse[F]
extension (x: NonEmptyTraverse.type) inline def derived[F[_]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse[F]
extension (x: SemigroupK.type) inline def derived[F[_]]: SemigroupK[F] = DerivedSemigroupK[F]
extension (x: MonoidK.type) inline def derived[F[_]]: MonoidK[F] = DerivedMonoidK[F]
extension (x: Contravariant.type) inline def derived[F[_]]: Contravariant[F] = DerivedContravariant[F]
extension (x: Invariant.type) inline def derived[F[_]]: Invariant[F] = DerivedInvariant[F]
extension (x: PartialOrder.type) inline def derived[A]: PartialOrder[A] = DerivedPartialOrder[A]
extension (x: ShowPretty.type) inline def derived[A]: ShowPretty[A] = DerivedShowPretty[A]
extension (x: Bifunctor.type) inline def derived[F[_, _]]: Bifunctor[F] = DerivedBifunctor[F]
extension (x: Bifoldable.type) inline def derived[F[_, _]]: Bifoldable[F] = DerivedBifoldable[F]
extension (x: Bitraverse.type) inline def derived[F[_, _]]: Bitraverse[F] = DerivedBitraverse[F]

object semiauto:
  inline def eq[A]: Eq[A] = DerivedEq[A]
  inline def hash[A]: Hash[A] = DerivedHash[A]
  inline def empty[A]: Empty[A] = DerivedEmpty[A]
  inline def semigroup[A]: Semigroup[A] = DerivedSemigroup[A]
  inline def monoid[A]: Monoid[A] = DerivedMonoid[A]
  inline def group[A]: Group[A] = DerivedGroup[A]
  inline def band[A]: Band[A] = DerivedBand[A]
  inline def order[A]: Order[A] = DerivedOrder[A]
  inline def commutativeSemigroup[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup[A]
  inline def commutativeMonoid[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid[A]
  inline def commutativeGroup[A]: CommutativeGroup[A] = DerivedCommutativeGroup[A]
  inline def semilattice[A]: Semilattice[A] = DerivedSemilattice[A]
  inline def boundedSemilattice[A]: BoundedSemilattice[A] = DerivedBoundedSemilattice[A]
  inline def applicative[F[_]]: Applicative[F] = DerivedApplicative[F]
  inline def apply[F[_]]: Apply[F] = DerivedApply[F]
  inline def nonEmptyAlternative[F[_]]: NonEmptyAlternative[F] = DerivedNonEmptyAlternative[F]
  inline def alternative[F[_]]: Alternative[F] = DerivedAlternative[F]
  inline def emptyK[F[_]]: EmptyK[F] = DerivedEmptyK[F]
  inline def pure[F[_]]: Pure[F] = DerivedPure[F]
  inline def foldable[F[_]]: Foldable[F] = DerivedFoldable[F]
  inline def functor[F[_]]: Functor[F] = DerivedFunctor[F]
  inline def reducible[F[_]]: Reducible[F] = DerivedReducible[F]
  inline def traverse[F[_]]: Traverse[F] = DerivedTraverse[F]
  inline def nonEmptyTraverse[F[_]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse[F]
  inline def show[A]: Show[A] = DerivedShow[A]
  inline def semigroupK[F[_]]: SemigroupK[F] = DerivedSemigroupK[F]
  inline def monoidK[F[_]]: MonoidK[F] = DerivedMonoidK[F]
  inline def contravariant[F[_]]: Contravariant[F] = DerivedContravariant[F]
  inline def invariant[F[_]]: Invariant[F] = DerivedInvariant[F]
  inline def partialOrder[A]: PartialOrder[A] = DerivedPartialOrder[A]
  inline def showPretty[A]: ShowPretty[A] = DerivedShowPretty[A]
  inline def bifunctor[F[_, _]]: Bifunctor[F] = DerivedBifunctor[F]
  inline def bifoldable[F[_, _]]: Bifoldable[F] = DerivedBifoldable[F]
  inline def bitraverse[F[_, _]]: Bitraverse[F] = DerivedBitraverse[F]

object strict:
  extension (x: Eq.type) inline def derived[A]: Eq[A] = DerivedEq.strict[A]
  extension (x: Hash.type) inline def derived[A]: Hash[A] = DerivedHash.strict[A]
  extension (x: PartialOrder.type) inline def derived[A]: PartialOrder[A] = DerivedPartialOrder.strict[A]
  extension (x: Order.type) inline def derived[A]: Order[A] = DerivedOrder.strict[A]
  extension (x: Show.type) inline def derived[A]: Show[A] = DerivedShow.strict[A]
  extension (x: ShowPretty.type) inline def derived[A]: ShowPretty[A] = DerivedShowPretty.strict[A]
  extension (x: Empty.type) inline def derived[A]: Empty[A] = DerivedEmpty.strict[A]
  extension (x: Semigroup.type) inline def derived[A]: Semigroup[A] = DerivedSemigroup.strict[A]
  extension (x: Monoid.type) inline def derived[A]: Monoid[A] = DerivedMonoid.strict[A]
  extension (x: Group.type) inline def derived[A]: Group[A] = DerivedGroup.strict[A]
  extension (x: Band.type) inline def derived[A]: Band[A] = DerivedBand.strict[A]
  extension (x: CommutativeSemigroup.type)
    inline def derived[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup.strict[A]
  extension (x: CommutativeMonoid.type) inline def derived[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid.strict[A]
  extension (x: CommutativeGroup.type) inline def derived[A]: CommutativeGroup[A] = DerivedCommutativeGroup.strict[A]
  extension (x: Semilattice.type) inline def derived[A]: Semilattice[A] = DerivedSemilattice.strict[A]
  extension (x: BoundedSemilattice.type)
    inline def derived[A]: BoundedSemilattice[A] = DerivedBoundedSemilattice.strict[A]
  extension (x: EmptyK.type) inline def derived[F[_]]: EmptyK[F] = DerivedEmptyK.strict[F]
  extension (x: SemigroupK.type) inline def derived[F[_]]: SemigroupK[F] = DerivedSemigroupK.strict[F]
  extension (x: MonoidK.type) inline def derived[F[_]]: MonoidK[F] = DerivedMonoidK.strict[F]
  extension (x: Pure.type) inline def derived[F[_]]: Pure[F] = DerivedPure.strict[F]
  extension (x: Invariant.type) inline def derived[F[_]]: Invariant[F] = DerivedInvariant.strict[F]
  extension (x: Functor.type) inline def derived[F[_]]: Functor[F] = DerivedFunctor.strict[F]
  extension (x: Contravariant.type) inline def derived[F[_]]: Contravariant[F] = DerivedContravariant.strict[F]
  extension (x: Apply.type) inline def derived[F[_]]: Apply[F] = DerivedApply.strict[F]
  extension (x: Applicative.type) inline def derived[F[_]]: Applicative[F] = DerivedApplicative.strict[F]
  extension (x: NonEmptyAlternative.type)
    inline def derived[F[_]]: NonEmptyAlternative[F] = DerivedNonEmptyAlternative.strict[F]
  extension (x: Alternative.type) inline def derived[F[_]]: Alternative[F] = DerivedAlternative.strict[F]
  extension (x: Foldable.type) inline def derived[F[_]]: Foldable[F] = DerivedFoldable.strict[F]
  extension (x: Reducible.type) inline def derived[F[_]]: Reducible[F] = DerivedReducible.strict[F]
  extension (x: Traverse.type) inline def derived[F[_]]: Traverse[F] = DerivedTraverse.strict[F]
  extension (x: NonEmptyTraverse.type) inline def derived[F[_]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse.strict[F]
  extension (x: Bifunctor.type) inline def derived[F[_, _]]: Bifunctor[F] = DerivedBifunctor.strict[F]
  extension (x: Bifoldable.type) inline def derived[F[_, _]]: Bifoldable[F] = DerivedBifoldable.strict[F]
  extension (x: Bitraverse.type) inline def derived[F[_, _]]: Bitraverse[F] = DerivedBitraverse.strict[F]

  object semiauto:
    inline def eq[A]: Eq[A] = DerivedEq.strict[A]
    inline def hash[A]: Hash[A] = DerivedHash.strict[A]
    inline def partialOrder[A]: PartialOrder[A] = DerivedPartialOrder.strict[A]
    inline def order[A]: Order[A] = DerivedOrder.strict[A]
    inline def show[A]: Show[A] = DerivedShow.strict[A]
    inline def showPretty[A]: ShowPretty[A] = DerivedShowPretty.strict[A]
    inline def empty[A]: Empty[A] = DerivedEmpty.strict[A]
    inline def semigroup[A]: Semigroup[A] = DerivedSemigroup.strict[A]
    inline def monoid[A]: Monoid[A] = DerivedMonoid.strict[A]
    inline def group[A]: Group[A] = DerivedGroup.strict[A]
    inline def band[A]: Band[A] = DerivedBand.strict[A]
    inline def commutativeSemigroup[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup.strict[A]
    inline def commutativeMonoid[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid.strict[A]
    inline def commutativeGroup[A]: CommutativeGroup[A] = DerivedCommutativeGroup.strict[A]
    inline def semilattice[A]: Semilattice[A] = DerivedSemilattice.strict[A]
    inline def boundedSemilattice[A]: BoundedSemilattice[A] = DerivedBoundedSemilattice.strict[A]
    inline def emptyK[F[_]]: EmptyK[F] = DerivedEmptyK.strict[F]
    inline def semigroupK[F[_]]: SemigroupK[F] = DerivedSemigroupK.strict[F]
    inline def monoidK[F[_]]: MonoidK[F] = DerivedMonoidK.strict[F]
    inline def pure[F[_]]: Pure[F] = DerivedPure.strict[F]
    inline def invariant[F[_]]: Invariant[F] = DerivedInvariant.strict[F]
    inline def functor[F[_]]: Functor[F] = DerivedFunctor.strict[F]
    inline def contravariant[F[_]]: Contravariant[F] = DerivedContravariant.strict[F]
    inline def apply[F[_]]: Apply[F] = DerivedApply.strict[F]
    inline def applicative[F[_]]: Applicative[F] = DerivedApplicative.strict[F]
    inline def nonEmptyAlternative[F[_]]: NonEmptyAlternative[F] = DerivedNonEmptyAlternative.strict[F]
    inline def alternative[F[_]]: Alternative[F] = DerivedAlternative.strict[F]
    inline def foldable[F[_]]: Foldable[F] = DerivedFoldable.strict[F]
    inline def reducible[F[_]]: Reducible[F] = DerivedReducible.strict[F]
    inline def traverse[F[_]]: Traverse[F] = DerivedTraverse.strict[F]
    inline def nonEmptyTraverse[F[_]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse.strict[F]
    inline def bifunctor[F[_, _]]: Bifunctor[F] = DerivedBifunctor.strict[F]
    inline def bifoldable[F[_, _]]: Bifoldable[F] = DerivedBifoldable.strict[F]
    inline def bitraverse[F[_, _]]: Bitraverse[F] = DerivedBitraverse.strict[F]

object auto:
  private type NotGiven0[F[_]] = [A] =>> NotGiven[F[A]]
  private type NotGiven1[T[_[_]]] = [F[_]] =>> NotGiven[T[F]]
  private type NotGiven2[T[_[_, _]]] = [F[_, _]] =>> NotGiven[T[F]]

  object eq:
    transparent inline given [A: NotGiven0[Eq]]: Eq[A] = DerivedEq[A]

  object hash:
    transparent inline given [A: NotGiven0[Hash]]: Hash[A] = DerivedHash[A]

  object empty:
    transparent inline given [A: NotGiven0[Empty]]: Empty[A] = DerivedEmpty[A]

  object semigroup:
    transparent inline given [A: NotGiven0[Semigroup]]: Semigroup[A] = DerivedSemigroup[A]

  object monoid:
    transparent inline given [A: NotGiven0[Monoid]]: Monoid[A] = DerivedMonoid[A]

  object group:
    transparent inline given [A: NotGiven0[Group]]: Group[A] = DerivedGroup[A]

  object band:
    transparent inline given [A: NotGiven0[Band]]: Band[A] = DerivedBand[A]

  object order:
    transparent inline given [A: NotGiven0[Order]]: Order[A] = DerivedOrder[A]

  object commutativeSemigroup:
    transparent inline given [A: NotGiven0[CommutativeSemigroup]]: CommutativeSemigroup[A] =
      DerivedCommutativeSemigroup[A]

  object commutativeMonoid:
    transparent inline given [A: NotGiven0[CommutativeMonoid]]: CommutativeMonoid[A] = DerivedCommutativeMonoid[A]

  object commutativeGroup:
    transparent inline given [A: NotGiven0[CommutativeGroup]]: CommutativeGroup[A] = DerivedCommutativeGroup[A]

  object semilattice:
    transparent inline given [A: NotGiven0[Semilattice]]: Semilattice[A] = DerivedSemilattice[A]

  object boundedSemilattice:
    transparent inline given [A: NotGiven0[BoundedSemilattice]]: BoundedSemilattice[A] = DerivedBoundedSemilattice[A]

  object show:
    transparent inline given [A: NotGiven0[Show]]: Show[A] = DerivedShow[A]

  object applicative:
    transparent inline given [F[_]: NotGiven1[Applicative]]: Applicative[F] = DerivedApplicative[F]

  object apply:
    transparent inline given [F[_]: NotGiven1[Apply]]: Apply[F] = DerivedApply[F]

  object nonEmptyAlternative:
    transparent inline given [F[_]: NotGiven1[NonEmptyAlternative]]: NonEmptyAlternative[F] =
      DerivedNonEmptyAlternative[F]

  object alternative:
    transparent inline given [F[_]: NotGiven1[Alternative]]: Alternative[F] = DerivedAlternative[F]

  object emptyK:
    transparent inline given [F[_]: NotGiven1[EmptyK]]: EmptyK[F] = DerivedEmptyK[F]

  object pure:
    transparent inline given [F[_]: NotGiven1[Pure]]: Pure[F] = DerivedPure[F]

  object functor:
    transparent inline given [F[_]: NotGiven1[Functor]]: Functor[F] = DerivedFunctor[F]

  object foldable:
    transparent inline given [F[_]: NotGiven1[Foldable]]: Foldable[F] = DerivedFoldable[F]

  object reducible:
    transparent inline given [F[_]: NotGiven1[Reducible]]: Reducible[F] = DerivedReducible[F]

  object traverse:
    transparent inline given [F[_]: NotGiven1[Traverse]]: Traverse[F] = DerivedTraverse[F]

  object nonEmptyTraverse:
    transparent inline given [F[_]: NotGiven1[NonEmptyTraverse]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse[F]

  object semigroupK:
    transparent inline given [F[_]: NotGiven1[SemigroupK]]: SemigroupK[F] = DerivedSemigroupK[F]

  object monoidK:
    transparent inline given [F[_]: NotGiven1[MonoidK]]: MonoidK[F] = DerivedMonoidK[F]

  object contravariant:
    transparent inline given [F[_]: NotGiven1[Contravariant]]: Contravariant[F] = DerivedContravariant[F]

  object invariant:
    transparent inline given [F[_]: NotGiven1[Invariant]]: Invariant[F] = DerivedInvariant[F]

  object partialOrder:
    transparent inline given [A: NotGiven0[PartialOrder]]: PartialOrder[A] = DerivedPartialOrder[A]

  object showPretty:
    transparent inline given [A: NotGiven0[Show]]: ShowPretty[A] = DerivedShowPretty[A]

  object bifunctor:
    transparent inline given [F[_, _]: NotGiven2[Bifunctor]]: Bifunctor[F] = DerivedBifunctor[F]

  object bifoldable:
    transparent inline given [F[_, _]: NotGiven2[Bifoldable]]: Bifoldable[F] = DerivedBifoldable[F]

  object bitraverse:
    transparent inline given [F[_, _]: NotGiven2[Bitraverse]]: Bitraverse[F] = DerivedBitraverse[F]
