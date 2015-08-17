package lms.util

import lms._

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * A CPS encoding of Option
 * an alternative to the struct representation
 */
trait OptionCPSOps
    extends Base
    with IfThenElse
    with BooleanOps
    with Equal
    with LiftVariables {
  import scala.language.implicitConversions


  abstract class OptionCPS[T: Manifest] { self =>

    def apply[X: Manifest](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X]

    def map[U: Manifest](f: Rep[T] => Rep[U]) = new OptionCPS[U] {
      def apply[X: Manifest](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) =
        self.apply(none, (t: Rep[T]) => some(f(t)))
    }

    def flatMap[U: Manifest](f: Rep[T] => OptionCPS[U]) = new OptionCPS[U] {
      def apply[X: Manifest](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) =
        self.apply(none, (t: Rep[T]) => f(t).apply(none, some))
    }

    def filter(p: Rep[T] => Rep[Boolean]) = new OptionCPS[T] {
      def apply[X: Manifest](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]) =
        self.apply(none, (t: Rep[T]) => if (p(t)) some(t) else none(()))
    }

    /**
     * isDefined does not make sense for this encoding
     */
  }

  /**
   * Companion object
   */
  object OptionCPS {
    def Some[T: Manifest](t: Rep[T]) = new OptionCPS[T] {
      def apply[X: Manifest](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X] =
        some(t)
    }

    def None[T: Manifest] = new OptionCPS[T] {
      def apply[X: Manifest](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X] =
        none(())
    }

    /**
     * a conditional expression for OptionCPS, mixed-stage
     * needs a different name than __ifThenElse because the latter requires
     * Rep `then` and `else` parameters
     */
  }


  /** helpers */
  def mkNone[T: Manifest]: Rep[OptionCPS[T]]
  def mkSome[T: Manifest](t: Rep[T]): Rep[OptionCPS[T]]

  /**
   * pimping my ride
   */
  implicit class OptionWrapperCls[T: Manifest](e: Rep[OptionCPS[T]]) {
    def apply[X: Manifest](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X]
     = option_apply(e, none, some)
  }

  /**
   * functions on Rep[OptionCPS]
   */
  def option_apply[X: Manifest, T: Manifest](
    e: Rep[OptionCPS[T]],
    none: Rep[Unit] => Rep[X],
    some: Rep[T] => Rep[X]
  ): Rep[X]

  def __ifThenElse[T: Manifest](
    cond: Rep[Boolean],
    thenp: => Rep[OptionCPS[T]],
    elsep: => Rep[OptionCPS[T]]
  ): Rep[OptionCPS[T]]

}

trait OptionCPSExp
    extends OptionCPSOps
    with BaseExp
    with IfThenElseExp
    with BooleanOpsExp
    with EqualExp {


  /**
   * Nodes representing OptionCPS. They act as wrappers mainly
   * There is also a special representation for conditionals involving
   * OptionCPS.
   */
  case class OptionWrapper[T](opt: OptionCPS[T]) extends Def[OptionCPS[T]]
  case class OptionCond[T](
    cond: Rep[Boolean],
    t: Exp[OptionCPS[T]],
    e: Exp[OptionCPS[T]]
  ) extends Def[OptionCPS[T]]

  /** helpers */
  def mkNone[T: Manifest]: Rep[OptionCPS[T]] = OptionWrapper(OptionCPS.None[T])
  def mkSome[T: Manifest](t: Rep[T]): Rep[OptionCPS[T]] = OptionWrapper(OptionCPS.Some(t))

  /**
   * Implementations of interface methods from above
   */
  def __ifThenElse[T: Manifest](
    cond: Rep[Boolean],
    thenp: => Rep[OptionCPS[T]],
    elsep: => Rep[OptionCPS[T]]
  ): Rep[OptionCPS[T]] = OptionCond(cond, thenp, elsep)

  /**
   * Wrapper around the apply function for OptionCPS
   * Also useful for special handling of conditionals
   */
  def option_apply[X: Manifest, T: Manifest](
    e: Rep[OptionCPS[T]],
    none: Rep[Unit] => Rep[X],
    some: Rep[T] => Rep[X]
  ): Rep[X] = e match {

    /* simple unwrapping */
    case Def(OptionWrapper(opt)) => opt.apply(none, some)

    /* The naive code gen for conditionals */
    case Def(OptionCond(cond, t, e)) =>

      val tmp = new OptionCPS[T] {
        def apply[X: Manifest](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]) = {
          if (cond) t(none, some) else e(none, some)
        }
      }
      tmp(none, some)

//    case OptionCond(cond, t, e) =>
//
//      import lms.ZeroVal
//      /**
//       * we create a new EitherCPS that handles conditionals
//       */
//      var isDefined = false; var value = ZeroVal[T]
//      val tmp = new EitherCPS[T] {
//        def apply[X: Manifest](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]) = {
//          if (cond)
//        }
//      }


  }

}
