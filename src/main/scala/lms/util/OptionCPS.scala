package lms.util

import lms._

import scala.lms.common._
import scala.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * A CPS encoding of Option
 * an alternative to the struct representation
 */
trait OptionCPS
    extends Base
    with IfThenElse
    with BooleanOps
    with LiftVariables
    with OptionOps
    with ZeroVal {

  /**
   * CPS encoding for Option
   * isDefined does not make sense for this encoding
   */
  abstract class OptionCPS[T: Typ] { self =>

    def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X]

    def map[U: Typ](f: Rep[T] => Rep[U]) = new OptionCPS[U] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) =
        self.apply(none, (t: Rep[T]) => some(f(t)))
    }

    def flatMap[U: Typ](f: Rep[T] => OptionCPS[U]) = new OptionCPS[U] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) =
        self.apply(none, (t: Rep[T]) => f(t).apply(none, some))
    }

    def filter(p: Rep[T] => Rep[Boolean]) = new OptionCPS[T] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]) =
        self.apply(none, (t: Rep[T]) => if (p(t)) some(t) else none(()))
    }

    /**
     * helper method that introduces vars and eventually yields a Rep[Option]
     */
    def toOption: Rep[Option[T]] = {
      var isDefined = unit(false); var value = zeroVal[T]
      self.apply(
        (_: Rep[Unit]) => unit(()),
        x => { isDefined = unit(true); value = x }
      )
      if (isDefined) make_opt(scala.Some(readVar(value))) else none[T]()
    }

  }

  /**
   * A node acting as a join point for OptionCPS
   */
  case class OptionCPSCond[T: Typ](
    cond: Rep[Boolean],
    t: OptionCPS[T],
    e: OptionCPS[T]
  ) extends OptionCPS[T] { self =>

    /**
     * naive apply function
     */
    def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X] =
      if (cond) t(none, some) else e(none, some)

    /**
     * overriding implementations for the usual suspects
     * for a conditional, we don't want to inline higher order functions
     * in each branch.
     * For options, this is handy especially if both sides of the conditional yield
     * a Some. Otherwise it does not really matter, because no computation is performed
     * in the None case anyway. While codegen may be suboptimal for the latter case,
     * it's a tradeoff worth taking.
     *
     * Or is it? Maybe it's a better idea to impose only monadic style
     * composition for Option, and special case the append/orElse function for
     * join points?
     * Well, here's a counterexample:
     * Some(x) flatMap { x => (if (cond(x)) Some(y) else Some(z)).bigcomputation }
     * with such code, we still need the conditional notation. I guess tradeoff is
     * where things stand at the moment.
     * Unless we want to analyse the body of each conditional expression, etc. etc.
     * Also, at the moment, I (manojo) don't know the performance implications for
     * either code style.
     */

    override def map[U: Typ](f: Rep[T] => Rep[U]) = new OptionCPS[U] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) = {
        var isDefined = unit(false); var value = zeroVal[T]

        self.apply(
          (_: Rep[Unit]) => unit(()),
          x => { isDefined = unit(true); value = x }
        )
        if (isDefined) some(f(value)) else none(unit(()))
      }
    }

    override def flatMap[U: Typ](f: Rep[T] => OptionCPS[U]) = new OptionCPS[U] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) = {
        var isDefined = unit(false); var value = zeroVal[T]

        self.apply(
          (_: Rep[Unit]) => unit(()),
          x => { isDefined = unit(true); value = x }
        )
        if (isDefined) f(value).apply(none, some) else none(unit(()))
      }
    }

    override def filter(p: Rep[T] => Rep[Boolean]) = new OptionCPS[T] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]) = {
        var isDefined = unit(false); var value = zeroVal[T]

        self.apply(
          (_: Rep[Unit]) => unit(()),
          x => { isDefined = unit(true); value = x }
        )
        if (isDefined && p(value)) some(value) else none(unit(()))
      }
    }

  }

  /**
   * Companion object
   */
  object OptionCPS {
    def Some[T: Typ](t: Rep[T]) = new OptionCPS[T] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X] =
        some(t)
    }

    def None[T: Typ] = new OptionCPS[T] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X] =
        none(())
    }

    /**
     * a conditional expression for OptionCPS, mixed-stage
     * needs a different name than __ifThenElse because the latter requires
     * Rep `then` and `else` parameters
     */
    def conditional[T: Typ](
      cond: Rep[Boolean],
      thenp: => OptionCPS[T],
      elsep: => OptionCPS[T]
    ): OptionCPS[T] = OptionCPSCond(cond, thenp, elsep)
  }
}
