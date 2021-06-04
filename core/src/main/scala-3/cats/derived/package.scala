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
  ReducibleDerivation,
  SemigroupDerivation,
  SemigroupKDerivation,
  ShowDerivation,
  TraverseDerivation,
  Instances:

  def foldable[F[_]](using ev: DerivedFoldable[F]): Foldable[F] = ev.instance

  def functor[F[_]](using ev: DerivedFunctor[F]): Functor[F] = ev.instance

  def reducible[F[_]](using ev: DerivedReducible[F]): Reducible[F] = ev.instance

  def traverse[F[_]](using ev: DerivedTraverse[F]): Traverse[F] = ev.instance


object auto:

  given [F[_]](using ev: DerivedFoldable[F]): Foldable[F] = ev.instance

  given [F[_]](using ev: DerivedFunctor[F]): Functor[F] = ev.instance

  given [F[_]](using ev: DerivedReducible[F]): Reducible[F] = ev.instance

  given [F[_]](using ev: DerivedTraverse[F]): Traverse[F] = ev.instance
