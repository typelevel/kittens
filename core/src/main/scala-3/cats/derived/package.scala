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

  inline def foldable[F[_]](using ev: DerivedFoldable[F]): Foldable[F] = ev.instance

  inline def functor[F[_]](using ev: DerivedFunctor[F]): Functor[F] = ev.instance

  inline def reducible[F[_]](using ev: DerivedReducible[F]): Reducible[F] = ev.instance

  inline def traverse[F[_]](using ev: DerivedTraverse[F]): Traverse[F] = ev.instance


object auto:

  inline given [F[_]](using ev: DerivedFoldable[F]): Foldable[F] = ev.instance

  inline given [F[_]](using ev: DerivedFunctor[F]): Functor[F] = ev.instance

  inline given [F[_]](using ev: DerivedReducible[F]): Reducible[F] = ev.instance

  inline given [F[_]](using ev: DerivedTraverse[F]): Traverse[F] = ev.instance
