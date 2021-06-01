package cats.derived

import cats.*

object semiauto extends
  CommutativeMonoidDerivation,
  CommutativeSemigroupDerivation,
  ContravariantDerivation,
  EmptyDerivation,
  EmptyKDerivation,
  EqDerivation,
  FoldableDerivation,
  FunctorDerivation,
  HashDerivation,
  InvariantDerivation,
  MonoidDerivation,
  MonoidKDerivation,
  OrderDerivation,
  PartialOrderDerivation,
  SemigroupDerivation,
  SemigroupKDerivation,
  ShowDerivation,
  TraverseDerivation,
  Instances:

  def foldable[F[_]](using ev: DerivedFoldable[F]): Foldable[F] = ev

  def functor[F[_]](using ev: DerivedFunctor[F]): Functor[F] = ev

  def reducible[F[_]](using ev: DerivedReducible[F]): Reducible[F] = ev

  def traverse[F[_]](using ev: DerivedTraverse[F]): Traverse[F] = ev


object auto:

  given [F[_]](using ev: DerivedFoldable[F]): Foldable[F] = ev

  given [F[_]](using ev: DerivedFunctor[F]): Functor[F] = ev

  given [F[_]](using ev: DerivedReducible[F]): Reducible[F] = ev

  given [F[_]](using ev: DerivedTraverse[F]): Traverse[F] = ev
