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

import scala.annotation.tailrec

import shapeless._

object iterable {
  object legacy {
    implicit def mkIterableLegacy[F[_], A](fa: F[A])(implicit mif: MkIterable[F]): Iterable[A] =
      mif.iterable(fa)
  }
}

trait MkIterable[F[_]] {
  import IterState._

  def initialState[A](fa: F[A]): IterState[A]

  def iterable[A](fa: F[A]): Iterable[A] =
    new Iterable[A] {
      lazy val initial: IterState[A] = initialState(fa)
      def iterator: Iterator[A] =
        new Iterator[A] {
          var first = initial
          var rest: List[() => IterState[A]] = Nil

          @tailrec
          def hasNext: Boolean =
            first match {
              case Return(_) => true
              case ReturnI(ia) =>
                if(ia.hasNext) true
                else if(rest.isEmpty) false
                else {
                  first = rest.head()
                  rest = rest.tail
                  hasNext
                }
              case Cont(hd, tl) =>
                first = hd
                rest = tl :: rest
                hasNext
              case Done =>
                if(rest.isEmpty) false
                else {
                  first = rest.head()
                  rest = rest.tail
                  hasNext
                }
            }

          def next: A = {
            if(!hasNext)
              throw new NoSuchElementException("next on empty iterator")
            (first: @unchecked) match {
              case Return(a) =>
                first = Done
                a
              case ReturnI(ia) =>
                ia.next
            }
          }
        }
    }
}

sealed trait IterState[+A]
object IterState {
  case class Return[A](a: A) extends IterState[A]
  case class ReturnI[A](ia: Iterator[A]) extends IterState[A]
  case class Cont[A](hd: IterState[A], tl: () => IterState[A]) extends IterState[A]
  case object Done extends IterState[Nothing]
}

object MkIterable extends MkIterable0 {
  import IterState._

  def apply[F[_]](implicit mif: MkIterable[F]): MkIterable[F] = mif

  implicit val mkIterableId: MkIterable[Id] =
    new MkIterable[Id] {
      def initialState[A](fa: A): IterState[A] = Return(fa)
    }

  implicit val mkIterableOption: MkIterable[Option] =
    new MkIterable[Option] {
      def initialState[A](fa: Option[A]): IterState[A] = ReturnI(fa.iterator)
    }

  implicit def mkIterableIterable[F[t] <: Iterable[t]]: MkIterable[F] =
    new MkIterable[F] {
      def initialState[A](fa: F[A]): IterState[A] =
        ReturnI(fa.iterator)
    }
}

trait MkIterable0 extends MkIterable1 {
  import IterState._

  implicit def mkIterableHcons[F[_]](implicit F: IsHCons1[F, MkIterable, MkIterable]): MkIterable[F] =
    new MkIterable[F] {
      def initialState[A](fa: F[A]): IterState[A] = {
        val (hd, tl) = F.unpack(fa)
        if(tl == HNil) F.fh.initialState(hd)
        else Cont(F.fh.initialState(hd), () => F.ft.initialState(tl))
      }
    }

  implicit def mkIterableCcons[F[_]](implicit F: IsCCons1[F, MkIterable, MkIterable]): MkIterable[F] =
    new MkIterable[F] {
      def initialState[A](fa: F[A]): IterState[A] = {
        F.unpack(fa) match {
          case Left(hd) => F.fh.initialState(hd)
          case Right(tl) => F.ft.initialState(tl)
        }
      }
    }
}

trait MkIterable1 extends MkIterable2 {
  import IterState._

  implicit def mkIterableCplit[F[_]](implicit split: Split1[F, MkIterable, MkIterable]): MkIterable[F] =
    new MkIterable[F] {
      def initialState[A](fa: F[A]): IterState[A] = {
        import split._
        ReturnI(fo.iterable(unpack(fa)).iterator.flatMap { ia => fi.iterable(ia).iterator })
      }
    }
}

trait MkIterable2 extends MkIterable3 {
  implicit def mkIterableGeneric[F[_]](implicit F: Generic1[F, MkIterable]): MkIterable[F] =
    new MkIterable[F] {
      def initialState[A](fa: F[A]): IterState[A] = F.fr.initialState(F.to(fa))
    }
}

trait MkIterable3 {
  import IterState._

  implicit def mkIterableConst[T]: MkIterable[Const[T]#λ] =
    new MkIterable[Const[T]#λ] {
      def initialState[A](fa: T): IterState[A] = Done
    }
}
