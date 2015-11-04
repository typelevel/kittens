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

import cats._, Fold.{ Continue, Pass, Return}, free.Trampoline, Trampoline.done, std.function._
import shapeless._

object foldable {
  implicit def apply[F[_]](implicit mff: WrappedOrphan[MkFoldable[F]]): Foldable[F] = mff.instance
}

trait MkFoldable[F[_]] extends Foldable[F] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = trampolinedFoldLeft(fa, b){ (b, a) => done(f(b, a)) }.run

  def partialFold[A, B](fa: F[A])(f: A => Fold[B]): Fold[B] = trampolinedPartialFold(fa){ a => done(f(a)) }.run

  def trampolinedFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Trampoline[B]): Trampoline[B]

  def trampolinedPartialFold[A, B](fa: F[A])(f: A => Trampoline[Fold[B]]): Trampoline[Fold[B]]
}

object MkFoldable extends MkFoldable0 {
  def apply[F[_]](implicit mff: MkFoldable[F]): MkFoldable[F] = mff

  implicit val id: MkFoldable[shapeless.Id] =
    new MkFoldable[shapeless.Id] {
      def trampolinedFoldLeft[A, B](fa: A, b: B)(f: (B, A) => Trampoline[B]): Trampoline[B] = f(b, fa)

      def trampolinedPartialFold[A, B](fa: A)(f: A => Trampoline[Fold[B]]): Trampoline[Fold[B]] = f(fa)
    }

  implicit def foldable[F[_]](implicit ff: Foldable[F]): MkFoldable[F] =
    new MkFoldable[F] {
      override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = ff.foldLeft(fa, b)(f)

      override def partialFold[A, B](fa: F[A])(f: A => Fold[B]): Fold[B] = ff.partialFold(fa)(f)

      def trampolinedFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Trampoline[B]): Trampoline[B] =
        done(foldLeft(fa, b){ (b, a) => f(b, a).run })

      def trampolinedPartialFold[A, B](fa: F[A])(f: A => Trampoline[Fold[B]]): Trampoline[Fold[B]] =
        done(partialFold(fa){ a => f(a).run })
    }
}

trait MkFoldable0 extends MkFoldable1 {
  // Induction step for products
  implicit def hcons[F[_]](implicit ihc: IsHCons1[F, MkFoldable, MkFoldable]): MkFoldable[F] =
    new MkFoldable[F] {
      def trampolinedFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Trampoline[B]): Trampoline[B] = {
        import ihc._
        val (hd, tl) = unpack(fa)
        for {
          h <- fh.trampolinedFoldLeft(hd, b)(f)
          t <- ft.trampolinedFoldLeft(tl, h)(f)
        } yield t
      }

      def trampolinedPartialFold[A, B](fa: F[A])(f: A => Trampoline[Fold[B]]): Trampoline[Fold[B]] = {
        import ihc._
        val (hd, tl) = unpack(fa)
        ft.trampolinedPartialFold(tl)(f).flatMap {
          case Continue(fbb) => fh.trampolinedPartialFold(hd)(f).map(_ compose fbb)
          case r: Return[b] => done(r)
          case _ => fh.trampolinedPartialFold(hd)(f)
        }
      }
    }

  // Induction step for coproducts
  implicit def ccons[F[_]](implicit icc: IsCCons1[F, MkFoldable, MkFoldable]): MkFoldable[F] =
    new MkFoldable[F] {
      def trampolinedFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Trampoline[B]): Trampoline[B] = {
        import icc._
        unpack(fa) match {
          case Left(hd)  => fh.trampolinedFoldLeft(hd, b)(f)
          case Right(tl) => ft.trampolinedFoldLeft(tl, b)(f)
        }
      }

      def trampolinedPartialFold[A, B](fa: F[A])(f: A => Trampoline[Fold[B]]): Trampoline[Fold[B]] = {
        import icc._
        unpack(fa) match {
          case Left(hd)  => fh.trampolinedPartialFold(hd)(f)
          case Right(tl) => ft.trampolinedPartialFold(tl)(f)
        }
      }
    }
}

trait MkFoldable1 extends MkFoldable2 {
  implicit def split[F[_]](implicit split: Split1[F, MkFoldable, MkFoldable]): MkFoldable[F] =
    new MkFoldable[F] {
      def trampolinedFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Trampoline[B]): Trampoline[B] = {
        import split._
        fo.trampolinedFoldLeft(unpack(fa), b)((bi, fai) => fi.trampolinedFoldLeft(fai, bi)(f))
      }

      def trampolinedPartialFold[A, B](fa: F[A])(f: A => Trampoline[Fold[B]]): Trampoline[Fold[B]] = {
        import split._
        fo.trampolinedPartialFold(unpack(fa))(fai => fi.trampolinedPartialFold(fai)(f))
      }
    }
}

trait MkFoldable2 extends MkFoldable3 {
  implicit def generic[F[_]](implicit gen: Generic1[F, MkFoldable]): MkFoldable[F] =
    new MkFoldable[F] {
      def trampolinedFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Trampoline[B]): Trampoline[B] =
        gen.fr.trampolinedFoldLeft(gen.to(fa), b)(f)

      def trampolinedPartialFold[A, B](fa: F[A])(f: A => Trampoline[Fold[B]]): Trampoline[Fold[B]] =
        gen.fr.trampolinedPartialFold(gen.to(fa))(f)
    }
}

trait MkFoldable3 {
  implicit def constFoldable[T]: MkFoldable[Const[T]#λ] =
    new MkFoldable[Const[T]#λ] {
      def trampolinedFoldLeft[A, B](fa: T, b: B)(f: (B, A) => Trampoline[B]): Trampoline[B] = done(b)

      def trampolinedPartialFold[A, B](fa: T)(f: A => Trampoline[Fold[B]]): Trampoline[Fold[B]] = done(Pass)
    }
}
