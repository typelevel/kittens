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

import cats.{ Eval, Functor }, Eval.now
import shapeless._


trait MkFunctor[F[_]] extends Functor[F] {
  def map[A, B](fa: F[A])(f: A => B): F[B] = safeMap(fa){ a => now(f(a)) }.value

  def safeMap[A, B](fa: F[A])(f: A => Eval[B]): Eval[F[B]]
}

object MkFunctor extends MkFunctorDerivation {
  def apply[F[_]](implicit mff: MkFunctor[F]): MkFunctor[F] = mff
}

private[derived] abstract class MkFunctorDerivation extends MkFunctor1 {
  // Induction step for products
  implicit def mkFunctorHcons[F[_]](implicit ihc: IsHCons1[F, Functor, MkFunctor]): MkFunctor[F] =
    new MkFunctor[F] {
      def safeMap[A, B](fa: F[A])(f: A => Eval[B]): Eval[F[B]] = {
        import ihc._
        val (hd, tl) = unpack(fa)
        for {
          fhd <- fh.safeMap(hd)(f)
          ftl <- ft.safeMap(tl)(f)
        } yield pack(fhd, ftl)
      }
    }

  // Induction step for coproducts
  implicit def mkFunctorCcons[F[_]](implicit icc: IsCCons1[F, Functor, MkFunctor]): MkFunctor[F] =
    new MkFunctor[F] {
      def safeMap[A, B](fa: F[A])(f: A => Eval[B]): Eval[F[B]] = {
        import icc._
        unpack(fa) match {
          case Left(hd)  => fh.safeMap(hd)(f).map { fhd => pack(Left(fhd)) }
          case Right(tl) => ft.safeMap(tl)(f).map { ftl => pack(Right(ftl)) }
        }
      }
    }
}

private[derived] abstract class  MkFunctor1 extends MkFunctor2 {
  // Further induction step for products todo: de-duplicate the code from the above induction with instance in scope
  implicit def mkFunctorHconsFurther[F[_]](implicit ihc: IsHCons1[F, MkFunctor, MkFunctor]): MkFunctor[F] =
    new MkFunctor[F] {
      def safeMap[A, B](fa: F[A])(f: A => Eval[B]): Eval[F[B]] = {
        import ihc._
        val (hd, tl) = unpack(fa)
        for {
          fhd <- fh.safeMap(hd)(f)
          ftl <- ft.safeMap(tl)(f)
        } yield pack(fhd, ftl)
      }
    }

  // Futher induction step for coproducts
  implicit def mkFunctorCconsFurther[F[_]](implicit icc: IsCCons1[F, MkFunctor, MkFunctor]): MkFunctor[F] =
    new MkFunctor[F] {
      def safeMap[A, B](fa: F[A])(f: A => Eval[B]): Eval[F[B]] = {
        import icc._
        unpack(fa) match {
          case Left(hd)  => fh.safeMap(hd)(f).map { fhd => pack(Left(fhd)) }
          case Right(tl) => ft.safeMap(tl)(f).map { ftl => pack(Right(ftl)) }
        }
      }
    }
}

private[derived] abstract class  MkFunctor2 extends MkFunctor3 {
  implicit def mkFunctorSplit[F[_]](implicit split: Split1[F, Functor, Functor]): MkFunctor[F] =
    new MkFunctor[F] {
      def safeMap[A, B](fa: F[A])(f: A => Eval[B]): Eval[F[B]] = {
        import split._
        fo.safeMap(unpack(fa))(fi.safeMap(_)(f)).map(pack)
      }
    }
}

private[derived] abstract class  MkFunctor3 extends MkFunctor4 {
  implicit def mkFunctorGeneric[F[_]](implicit gen: Generic1[F, MkFunctor]): MkFunctor[F] =
    new MkFunctor[F] {
      def safeMap[A, B](fa: F[A])(f: A => Eval[B]): Eval[F[B]] =
        gen.fr.safeMap(gen.to(fa))(f).map(gen.from)
    }
}

private[derived] abstract class  MkFunctor4 {
  implicit def mkFunctorConstFunctor[T]: MkFunctor[Const[T]#λ] =
    new MkFunctor[Const[T]#λ] {
      def safeMap[A, B](t: T)(f: A => Eval[B]): Eval[T] = now(t)
    }

  implicit class FunctorSafeMap[F[_]](val ff: Functor[F]) {
    def safeMap[A, B](fa: F[A])(f: A => Eval[B]): Eval[F[B]] =
      ff match {
        case mff: MkFunctor[F] => mff.safeMap(fa)(f)
        case _ => now(ff.map(fa){ a => f(a).value })
      }
  }
}
