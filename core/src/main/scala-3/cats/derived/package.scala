package cats.derived

import cats.*

extension (F: Foldable.type)
  inline def derived[F[_]]: Foldable[F] = DerivedFoldable[F]

extension (F: Functor.type)
  inline def derived[F[_]]: Functor[F] = DerivedFunctor[F]

extension (F: Reducible.type)
  inline def derived[F[_]]: Reducible[F] = DerivedReducible[F]

extension (F: Traverse.type)
  inline def derived[F[_]]: Traverse[F] = DerivedTraverse[F]

object semiauto extends
  CommutativeMonoidDerivation,
  CommutativeSemigroupDerivation,
  ContravariantDerivation,
  EmptyDerivation,
  EmptyKDerivation,
  EqDerivation,
  HashDerivation,
  InvariantDerivation,
  MonoidDerivation,
  MonoidKDerivation,
  OrderDerivation,
  PartialOrderDerivation,
  SemigroupDerivation,
  SemigroupKDerivation,
  ShowDerivation,
  Instances:

  inline def foldable[F[_]](using ev: DerivedFoldable[F]): Foldable[F] = ev.instance

  inline def functor[F[_]](using ev: DerivedFunctor[F]): Functor[F] = ev.instance

  inline def reducible[F[_]](using ev: DerivedReducible[F]): Reducible[F] = ev.instance

  inline def traverse[F[_]](using ev: DerivedTraverse[F]): Traverse[F] = ev.instance


object auto:

  inline given [F[_]]: Foldable[F] = DerivedFoldable[F]

  inline given [F[_]]: Functor[F] = DerivedFunctor[F]

  inline given [F[_]]: Reducible[F] = DerivedReducible[F]

  inline given [F[_]]: Traverse[F] = DerivedTraverse[F]
