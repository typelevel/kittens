package pkg

trait IListDefn {
  sealed trait IList[A]
  case class INil[A]() extends IList[A]
  case class ICons[A](h: A, t: IList[A]) extends IList[A]
}
