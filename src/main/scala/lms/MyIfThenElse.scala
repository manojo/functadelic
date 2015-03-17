package lms

import scala.virtualization.lms.common._

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{ GenericNestedCodegen, GenericFatCodegen }
import scala.reflect.{ SourceContext, RefinedManifest }

import java.io.PrintWriter

/**
 * Extra optimisations for IfThenElse. Based on code at https://github.com/manojo/experiments
 */
trait MyIfThenElseExpOpt
    extends IfThenElseExpOpt
    with BooleanOpsExp
    with EqualExpBridge {

  /**
   * A map that records conditions already seen. So that inner conditional expressions using
   * the same condition are eliminated.
   */
  val map = new scala.collection.mutable.HashMap[Rep[Boolean], Boolean]

  override def __ifThenElse[T: Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])
                                        (implicit pos: SourceContext) = cond match {
    case Const(true) => thenp
    case Const(false) => elsep
    case Def(BooleanNegate(a)) => __ifThenElse(a, elsep, thenp)
    case Def(NotEqual(a, b)) => __ifThenElse(equals(a, b), elsep, thenp)
    case _ =>
      if (map.contains(cond)) {
        if (map(cond)) thenp else elsep
      } else {
        val a = reifyEffectsHere {
          map += (cond) -> true
          //the by name parameter is now evaluated
          //thereby triggering possible nested ifThenElse-s
          val tmp = thenp
          map -= cond
          tmp
        }

        val b = reifyEffectsHere {
          map += (cond) -> false
          val tmp = elsep
          map -= cond
          tmp
        }

        ifThenElse(cond, a, b)
      }
  }
}
