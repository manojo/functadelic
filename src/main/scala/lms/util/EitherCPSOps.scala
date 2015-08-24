package lms.util

import lms._

import scala.lms.common._
import scala.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * A CPS encoding of Either
 * To see whether CPS encoding + code gen generates good code.
 *
 * Extending BaseExp just to be able to extend `Def`.
 * This allows us to have a Rep[EitherCPS]
 *
 * see http://manojo.github.io/2015/03/20/cps-encoding-either/ for more
 * details
 */
trait EitherCPSOps
    extends Base
    with IfThenElse
    with BooleanOps
    with LiftVariables
    with ZeroVal {

  /**
   * A CPS encoding of Either: Either is a construct that takes a value
   * of type A or B and eventually produces a value of type X
   * This implementation is not directly used here, but is wrapped inside
   * and `EitherWrapper`, so that it's accessible in the `Exp` world.
   */
  abstract class EitherCPS[A: Typ, B: Typ] { self =>

    def apply[X: Typ](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]): Rep[X]

    def map[C: Typ, D: Typ](lmap: Rep[A] => Rep[C], rmap: Rep[B] => Rep[D])
      = new EitherCPS[C, D] {

        def apply[X: Typ](lf: Rep[C] => Rep[X], rf: Rep[D] => Rep[X]) = self.apply(
          a => lf(lmap(a)),
          b => rf(rmap(b))
        )
      }
  }

  /**
   * Companion object
   */
  object EitherCPS {

    def LeftCPS[A: Typ, B: Typ](a: Rep[A]) = new EitherCPS[A, B] {
      def apply[X: Typ](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]) =
        lf(a)
    }

    def RightCPS[A: Typ, B: Typ](b: Rep[B]) = new EitherCPS[A, B] {
      def apply[X: Typ](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]) =
        rf(b)
    }


    def conditional[A: Typ, B: Typ](
      cond: Rep[Boolean],
      thenp: => EitherCPS[A, B],
      elsep: => EitherCPS[A, B]
    )(implicit leftValue: Option[Var[A]],
               rightValue: Option[Var[B]]): EitherCPS[A, B] = {

      new EitherCPS[A, B] {
        def apply[X: Typ](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]) = {

          import lms.ZeroVal

          var l = zeroVal[A]; var r = zeroVal[B]

          var isLeft = true
          val lCont = (a: Rep[A]) => {
            if (leftValue.isDefined) {
              leftValue.get = a
            } else { l = a }
            isLeft = true
          }
          //val lCont = (a: Rep[A]) => { l = a; isLeft = true }
          val rCont = (b: Rep[B]) => {
            if (rightValue.isDefined) {
              rightValue.get = b
            } else { r = b }
            isLeft = false
          }
          //val rCont = (b: Rep[B]) => { r = b; isLeft = false }
          if (cond) thenp.apply[Unit](lCont, rCont)
          else elsep.apply[Unit](lCont, rCont)


          if (isLeft) {
            val leftVar =
              if (leftValue.isDefined) leftValue.get
              else l
            lf(leftVar)
          } else {
            val rightVar =
              if (rightValue.isDefined) rightValue.get
              else r
            rf(rightVar)
          }
        }
      }
    }
  }

  /**
   * Pimping my ride, so I can write DSL style code at the interface
   */
  implicit class EitherCPSCls[A: Typ, B: Typ](e: Rep[EitherCPS[A, B]]) {
    def map[C: Typ, D: Typ](
      lmap: Rep[A] => Rep[C],
      rmap: Rep[B] => Rep[D]
    ): Rep[EitherCPS[C, D]] = eitherCPS_map(e, lmap, rmap)

    def apply[X: Typ](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]): Rep[X] =
      either_apply(e, lf, rf)
  }

  /**
   * interface-level functions
   */
  def mkLeft[A: Typ, B: Typ](a: Rep[A]): Rep[EitherCPS[A, B]]
  def mkRight[A: Typ, B: Typ](b: Rep[B]): Rep[EitherCPS[A, B]]

  def eitherCPS_map[A: Typ, B: Typ, C: Typ, D: Typ](
    e: Rep[EitherCPS[A, B]],
    lmap: Rep[A] => Rep[C],
    rmap: Rep[B] => Rep[D]
  ): Rep[EitherCPS[C, D]]

  def either_apply[A: Typ, B: Typ, X: Typ](
    e: Rep[EitherCPS[A, B]],
    lf: Rep[A] => Rep[X],
    rf: Rep[B] => Rep[X]
  ): Rep[X]

  def either_conditional[A: Typ, B: Typ](
    cond: Rep[Boolean],
    thenp: => Rep[EitherCPS[A, B]],
    elsep: => Rep[EitherCPS[A, B]]
  )(implicit leftValue: Option[Var[A]],
             rightValue: Option[Var[B]]): Rep[EitherCPS[A, B]]

  def __ifThenElse[A: Typ, B: Typ](
    cond: Rep[Boolean],
    thenp: => Rep[EitherCPS[A, B]],
    elsep: => Rep[EitherCPS[A, B]]
  )(implicit leftValue: Option[Var[A]],
             rightValue: Option[Var[B]]) = either_conditional(cond, thenp, elsep)
}

trait EitherCPSOpsExp
    extends EitherCPSOps
    with BaseExp
    with IfThenElseExp
    with BooleanOpsExp
    with EqualExp
    with ZeroValExp
    /** this trait should be mixed in higher up */ with PrimitiveOpsExp {

  import EitherCPS._

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def eithercps_typ[A: Typ, B: Typ]: Typ[EitherCPS[A, B]] = {
    implicit val ManifestTyp(mA) = typ[A]
    implicit val ManifestTyp(mB) = typ[B]
    manifestTyp
  }

  /**
   * The wrapper acts as a Rep[EitherCPS[A, B]]
   */
  case class EitherWrapper[A, B](e: EitherCPS[A, B]) extends Def[EitherCPS[A, B]]

  def mkLeft[A: Typ, B: Typ](a: Rep[A]): Rep[EitherCPS[A, B]] =
    EitherWrapper(LeftCPS[A, B](a))

  def mkRight[A: Typ, B: Typ](b: Rep[B]): Rep[EitherCPS[A, B]] =
    EitherWrapper(RightCPS[A, B](b))

  /**
   * Both the functions below will misbehave if we have some other representation
   * of `EitherCPS`. Which may be uncool at codegen time. But then again,
   * if that happens, we are probably doing something wrong-ish, so it's kind
   * of a sanity check
   */
  def eitherCPS_map[A: Typ, B: Typ, C: Typ, D: Typ](
    e: Rep[EitherCPS[A, B]],
    lmap: Rep[A] => Rep[C],
    rmap: Rep[B] => Rep[D]
  ): Rep[EitherCPS[C, D]] = e match {
    case Def(EitherWrapper(sth)) => EitherWrapper(sth map (lmap, rmap))
  }

  def either_apply[A: Typ, B: Typ, X: Typ](
    e: Rep[EitherCPS[A, B]],
    lf: Rep[A] => Rep[X],
    rf: Rep[B] => Rep[X]
  ): Rep[X] = e match {
    case Def(EitherWrapper(sth)) => sth.apply(lf, rf)
  }

  /**
   * a 'conditional' either
   * lifts conditional expressions to Either level
   *
   * Note: this implementation works only because we are
   * evaluating `thenp` and `elsep` here, and they are simple expressions
   * If they are blocks, the pattern match will fail.
   */
  def either_conditional[A: Typ, B: Typ](
    cond: Rep[Boolean],
    thenp: => Rep[EitherCPS[A, B]],
    elsep: => Rep[EitherCPS[A, B]]
  )(implicit leftValue: Option[Var[A]],
               rightValue: Option[Var[B]]): Rep[EitherCPS[A, B]] = (thenp, elsep) match { //stricting them here
    case (Def(EitherWrapper(t)), Def(EitherWrapper(e))) =>
      EitherWrapper(conditional(cond, t, e))
  }
}
