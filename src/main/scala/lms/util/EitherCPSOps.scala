package lms.util

import lms._

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * A CPS encoding of Either
 * To see whether CPS encoding + code gen generates good code.
 *
 * Extending BaseExp just to be able to extend `Def`.
 * This allows us to have a Rep[EitherBis]
 */
trait EitherCPSOps extends Base with IfThenElse with BooleanOps {
  import scala.language.implicitConversions

  /**
   * A CPS encoding of Either: Either is a construct that takes a value
   * of type A or B and eventually produces a value of type X
   * This implementation is not directly used here, but is wrapped inside
   * and `EitherWrapper`, so that it's accessible in the `Exp` world.
   */
  abstract class EitherCPS[A: Manifest, B: Manifest] { self =>

    def apply[X: Manifest](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]): Rep[X]

    def map[C: Manifest, D: Manifest](lmap: Rep[A] => Rep[C], rmap: Rep[B] => Rep[D])
      = new EitherCPS[C, D] {

        def apply[X: Manifest](lf: Rep[C] => Rep[X], rf: Rep[D] => Rep[X]) = self.apply(
          a => lf(lmap(a)),
          b => rf(rmap(b))
        )
      }
  }

  /**
   * Companion object
   */
  object EitherCPS {

    def LeftCPS[A: Manifest, B: Manifest](a: Rep[A]) = new EitherCPS[A, B] {
      def apply[X: Manifest](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]) =
        lf(a)
    }

    def RightCPS[A: Manifest, B: Manifest](b: Rep[B]) = new EitherCPS[A, B] {
      def apply[X: Manifest](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]) =
        rf(b)
    }

    def conditional[A: Manifest, B: Manifest](
      cond: Rep[Boolean],
      thenp: => EitherCPS[A, B],
      elsep: => EitherCPS[A, B]
    ): EitherCPS[A, B] = new EitherCPS[A, B] {
      def apply[X: Manifest](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]) =
        if (cond) thenp.apply(lf, rf) else elsep.apply(lf, rf)
    }
  }

  /**
   * A ghost type, called `EitherBis`
   * to distinguish from the other `Either`
   */
  class EitherBis[A, B]

  /**
   * Pimping my ride, so I can write DSL style code at the interface
   */
  implicit class EitherBisCls[A: Manifest, B: Manifest](e: Rep[EitherBis[A, B]]) {
    def map[C: Manifest, D: Manifest](
      lmap: Rep[A] => Rep[C],
      rmap: Rep[B] => Rep[D]
    ): Rep[EitherBis[C, D]] = eitherCPS_map(e, lmap, rmap)

    def apply[X: Manifest](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]): Rep[X] =
      either_apply(e, lf, rf)
  }

  /**
   * interface-level functions
   */
  def mkLeft[A: Manifest, B: Manifest](a: Rep[A]): Rep[EitherBis[A, B]]
  def mkRight[A: Manifest, B: Manifest](b: Rep[B]): Rep[EitherBis[A, B]]

  def eitherCPS_map[A: Manifest, B: Manifest, C: Manifest, D: Manifest](
    e: Rep[EitherBis[A, B]],
    lmap: Rep[A] => Rep[C],
    rmap: Rep[B] => Rep[D]
  ): Rep[EitherBis[C, D]]

  def either_apply[A: Manifest, B: Manifest, X: Manifest](
    e: Rep[EitherBis[A, B]],
    lf: Rep[A] => Rep[X],
    rf: Rep[B] => Rep[X]
  ): Rep[X]

  def either_conditional[A: Manifest, B: Manifest](
    cond: Rep[Boolean],
    thenp: => Rep[EitherBis[A, B]],
    elsep: => Rep[EitherBis[A, B]]
  ): Rep[EitherBis[A, B]]
}

trait EitherCPSOpsExp extends EitherCPSOps
    with BaseExp
    with IfThenElseExpOpt
    with BooleanOpsExpOpt
    with EqualExp {

  import EitherCPS._

  /**
   * The wrapper acts as a Rep[EitherBis[A, B]]
   */
  case class EitherWrapper[A, B](e: EitherCPS[A, B]) extends Def[EitherBis[A, B]]

  def mkLeft[A: Manifest, B: Manifest](a: Rep[A]): Rep[EitherBis[A, B]] =
    EitherWrapper(LeftCPS[A, B](a))

  def mkRight[A: Manifest, B: Manifest](b: Rep[B]): Rep[EitherBis[A, B]] =
    EitherWrapper(RightCPS[A, B](b))

  /**
   * Both the functions below will misbehave if we have some other representation
   * of `EitherBis`. Which may be uncool at codegen time. But then again,
   * if that happens, we are probably doing something wrong-ish, so it's kind
   * of a sanity check
   */
  def eitherCPS_map[A: Manifest, B: Manifest, C: Manifest, D: Manifest](
    e: Rep[EitherBis[A, B]],
    lmap: Rep[A] => Rep[C],
    rmap: Rep[B] => Rep[D]
  ): Rep[EitherBis[C, D]] = e match {
    case Def(EitherWrapper(sth)) => EitherWrapper(sth map (lmap, rmap))
  }

  def either_apply[A: Manifest, B: Manifest, X: Manifest](
    e: Rep[EitherBis[A, B]],
    lf: Rep[A] => Rep[X],
    rf: Rep[B] => Rep[X]
  ): Rep[X] = e match {
    case Def(EitherWrapper(sth)) => sth.apply(lf, rf)
  }

  /**
   * a 'conditional' parser
   * lifts conditional expressions to Either level
   *
   * Note: this implementation works only because we are
   * evaluating `thenp` and `elsep` here, and they are simple expressions
   * If they are blocks, the pattern match will fail.
   */
  def either_conditional[A: Manifest, B: Manifest](
    cond: Rep[Boolean],
    thenp: => Rep[EitherBis[A, B]],
    elsep: => Rep[EitherBis[A, B]]
  ): Rep[EitherBis[A, B]] = (thenp, elsep) match { //stricting them here
    case (Def(EitherWrapper(t)), Def(EitherWrapper(e))) =>
      EitherWrapper(conditional(cond, t, e))
  }
}
