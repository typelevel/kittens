package cats.derived

import cats.{Eq, Traverse}
import cats.laws.discipline.*
import scala.compiletime.*

class TraverseSuite extends KittensSuite:
  import ADTs.*
  import TraverseSuite.*

  inline def tests[F[_]]: TraverseTests[F] =
    TraverseTests[F](summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[IList]", tests[IList].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[Tree]", tests[Tree].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[GenericAdt]", tests[GenericAdt].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[OptList]", tests[OptList].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[ListSnoc]", tests[ListSnoc].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[AndChar]", tests[AndChar].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[EnumK1]", tests[EnumK1].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[Many]", tests[Many].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[AtMostOne]", tests[AtMostOne].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[AtLeastOne]", tests[AtLeastOne].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Traverse[Tree]]))

  locally {
    import auto.traverse.given
    validate("auto.traverse")
  }

  locally {
    import semiInstances.given
    validate("semiauto.traverse")
  }

  locally {
    import derivedInstances.*
    val instance = "derived.traverse"
    checkAll(s"$instance[IList]", tests[IList].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[Tree]", tests[Tree].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[GenericAdt]", tests[GenericAdt].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[EnumK1]", tests[EnumK1].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[Many]", tests[Many].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[AtMostOne]", tests[AtMostOne].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance[AtLeastOne]", tests[AtLeastOne].traverse[Int, Double, String, Long, Option, Option])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Traverse[Tree]))
  }

end TraverseSuite

object TraverseSuite:
  import ADTs.*

  type OptList[A] = Option[List[A]]
  type ListSnoc[A] = List[Snoc[A]]
  type AndChar[A] = (A, Char)

  object semiInstances:
    given Traverse[IList] = semiauto.traverse
    given Traverse[Tree] = semiauto.traverse
    given Traverse[GenericAdt] = semiauto.traverse
    given Traverse[OptList] = semiauto.traverse
    given Traverse[ListSnoc] = semiauto.traverse
    given Traverse[AndChar] = semiauto.traverse
    given Traverse[Interleaved] = semiauto.traverse
    given Traverse[EnumK1] = semiauto.traverse
    given Traverse[Many] = semiauto.traverse
    given Traverse[AtLeastOne] = semiauto.traverse
    given Traverse[AtMostOne] = semiauto.traverse

  object derivedInstances:
    case class IList[A](x: ADTs.IList[A]) derives Traverse
    case class Tree[A](x: ADTs.Tree[A]) derives Traverse
    case class GenericAdt[A](x: ADTs.GenericAdt[A]) derives Traverse
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives Traverse
    case class EnumK1[A](x: ADTs.EnumK1[A]) derives Traverse
    case class Many[A](x: ADTs.Many[A]) derives Traverse
    case class AtMostOne[A](x: ADTs.AtMostOne[A]) derives Traverse
    case class AtLeastOne[A](x: ADTs.AtLeastOne[A]) derives Traverse

end TraverseSuite
