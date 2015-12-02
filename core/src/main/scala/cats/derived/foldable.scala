/*
 * Copyright (c) 2015 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.derived

import cats.{ Eval, Foldable }, Eval.now
import export.{ exports, imports, reexports }
import shapeless._

@reexports[MkFoldable]
object foldable {
  @imports[Foldable]
  object legacy
}

trait MkFoldable[F[_]] extends Foldable[F] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = safeFoldLeft(fa, b){ (b, a) => now(f(b, a)) }.value

  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B]
}

@exports
object MkFoldable extends MkFoldable0 {
  def apply[F[_]](implicit mff: MkFoldable[F]): MkFoldable[F] = mff

  implicit val id: MkFoldable[shapeless.Id] =
    new MkFoldable[shapeless.Id] {
      def foldRight[A, B](fa: A, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa, lb)

      def safeFoldLeft[A, B](fa: A, b: B)(f: (B, A) => Eval[B]): Eval[B] = now(f(b, fa).value)
    }
}

trait MkFoldable0 extends MkFoldable1 {
  // Induction step for products
  implicit def hcons[F[_]](implicit ihc: IsHCons1[F, Foldable, MkFoldable]): MkFoldable[F] =
    new MkFoldable[F] {
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        import ihc._
        val (hd, tl) = unpack(fa)
        for {
          t <- ft.foldRight(tl, lb)(f)
          h <- fh.foldRight(hd, now(t))(f)
        } yield h
      }

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] = {
        import ihc._
        val (hd, tl) = unpack(fa)
        for {
          h <- fh.safeFoldLeft(hd, b)(f)
          t <- ft.safeFoldLeft(tl, h)(f)
        } yield t
      }
    }

  // Induction step for coproducts
  implicit def ccons[F[_]](implicit icc: IsCCons1[F, Foldable, MkFoldable]): MkFoldable[F] =
    new MkFoldable[F] {
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        import icc._
        unpack(fa) match {
          case Left(hd)  => fh.foldRight(hd, lb)(f)
          case Right(tl) => ft.foldRight(tl, lb)(f)
        }
      }

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] = {
        import icc._
        unpack(fa) match {
          case Left(hd)  => fh.safeFoldLeft(hd, b)(f)
          case Right(tl) => ft.safeFoldLeft(tl, b)(f)
        }
      }
    }
}

trait MkFoldable1 extends MkFoldable2 {
  implicit def split[F[_]](implicit split: Split1[F, Foldable, Foldable]): MkFoldable[F] =
    new MkFoldable[F] {
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        import split._
        fo.foldRight(unpack(fa), lb) { (fai, lbi) => fi.foldRight(fai, lbi)(f) }
      }

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] = {
        import split._
        fo.safeFoldLeft(unpack(fa), b){ (lbi, fai) => fi.safeFoldLeft(fai, lbi)(f) }
      }
    }
}

trait MkFoldable2 extends MkFoldable3 {
  implicit def generic[F[_]](implicit gen: Generic1[F, MkFoldable]): MkFoldable[F] =
    new MkFoldable[F] {
      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        gen.fr.foldRight(gen.to(fa), lb)(f)

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] =
        gen.fr.safeFoldLeft(gen.to(fa), b)(f)
    }
}

trait MkFoldable3 {
  implicit def constFoldable[T]: MkFoldable[Const[T]#λ] =
    new MkFoldable[Const[T]#λ] {
      def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

      def safeFoldLeft[A, B](fa: T, b: B)(f: (B, A) => Eval[B]): Eval[B] = now(b)
    }

  implicit class FoldableSafeFoldLeft[F[_]](val ff: Foldable[F]) {
    def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] =
      ff match {
        case mff: MkFoldable[F] => mff.safeFoldLeft(fa, b)(f)
        case _ => now(ff.foldLeft(fa, b) { (b, a) => f(b, a).value })
      }
  }
}
