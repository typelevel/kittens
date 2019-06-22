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

import shapeless._

import scala.annotation.{implicitNotFound, tailrec}

@implicitNotFound("Could not convert ${F} to an Iterable")
trait MkIterable[F[_]] {
  def initialState[A](fa: F[A]): IterState[A]

  def iterable[A](fa: F[A]): Iterable[A] = new Iterable[A] {
    def iterator: Iterator[A] = new Iterator[A] {
      var first = initialState(fa)
      var rest: List[() => IterState[A]] = Nil

      @tailrec def hasNext: Boolean = first match {
        case IterState.Return(_) => true
        case IterState.Iterate(it) =>
          it.hasNext || rest.nonEmpty && {
            first = rest.head()
            rest = rest.tail
            hasNext
          }
        case IterState.Cont(head, tail) =>
          first = head
          rest = tail :: rest
          hasNext
        case IterState.Done =>
          rest.nonEmpty && {
            first = rest.head()
            rest = rest.tail
            hasNext
          }
      }

      def next: A =
        if (!hasNext) Iterator.empty.next()
        else (first: @unchecked) match {
          case IterState.Return(a) =>
            first = IterState.Done
            a
          case IterState.Iterate(it) =>
            it.next
        }
    }
  }
}

sealed trait IterState[+A]
object IterState {
  final case class Return[A](a: A) extends IterState[A]
  final case class Iterate[A](it: Iterator[A]) extends IterState[A]
  final case class Cont[A](head: IterState[A], tail: () => IterState[A]) extends IterState[A]
  case object Done extends IterState[Nothing]
}

object MkIterable extends MkIterableDerivation {
  def apply[F[_]](implicit F: MkIterable[F]): MkIterable[F] = F
}

private[derived] abstract class MkIterableDerivation extends MkIterableNested {
  implicit val mkIterableHNil: MkIterable[Const[HNil]#λ] = mkIterableConst
  implicit val mkIterableCNil: MkIterable[Const[CNil]#λ] = mkIterableConst

  implicit val mkIterableId: MkIterable[Id] =
    new MkIterable[Id] {
      def initialState[A](fa: A) = IterState.Return(fa)
    }

  implicit val mkIterableOption: MkIterable[Option] =
    new MkIterable[Option] {
      def initialState[A](fa: Option[A]) = IterState.Iterate(fa.iterator)
    }

  implicit def mkIterableIterable[F[t] <: Iterable[t]]: MkIterable[F] =
    new MkIterable[F] {
      def initialState[A](fa: F[A]) = IterState.Iterate(fa.iterator)
    }

  implicit def mkIterableConst[T]: MkIterable[Const[T]#λ] =
    new MkIterable[Const[T]#λ] {
      def initialState[A](fa: T) = IterState.Done
    }
}

private[derived] abstract class MkIterableNested extends MkIterableGeneric {

  implicit def mkIterableNested[F[_]](implicit F: Split1[F, MkIterable, MkIterable]): MkIterable[F] =
    new MkIterable[F] {

      def initialState[A](fa: F[A]) =
        IterState.Iterate(F.fo.iterable(F.unpack(fa)).iterator.flatMap(F.fi.iterable(_).iterator))
    }
}

private[derived] abstract class MkIterableGeneric {

  implicit def mkIterableHCons[F[_]](implicit F: IsHCons1[F, MkIterable, MkIterable]): MkIterable[F] =
    new MkIterable[F] {

      def initialState[A](fa: F[A]) = {
        val (fha, fta) = F.unpack(fa)
        IterState.Cont(F.fh.initialState(fha), () => F.ft.initialState(fta))
      }
    }

  implicit def mkIterableCCons[F[_]](implicit F: IsCCons1[F, MkIterable, MkIterable]): MkIterable[F] =
    new MkIterable[F] {

      def initialState[A](fa: F[A]) = F.unpack(fa) match {
        case Left(fha) => F.fh.initialState(fha)
        case Right(fta) => F.ft.initialState(fta)
      }
    }

  implicit def mkIterableGeneric[F[_]](implicit F: Generic1[F, MkIterable]): MkIterable[F] =
    new MkIterable[F] {
      def initialState[A](fa: F[A]) = F.fr.initialState(F.to(fa))
    }
}
