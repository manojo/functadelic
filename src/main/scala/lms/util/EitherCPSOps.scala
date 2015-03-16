package lms.util

import lms._

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * A CPS encoding of Either
 * To see whether CPS encoding + code gen generates good code.
 */

trait EitherCPSOps extends Base with BooleanOps {
  import scala.language.implicitConversions

  /**
   * A CPS encoding of Either: Either is a construct that takes a value
   * of type A or B and eventually produces a value of type X
   */
  abstract class EitherCPS[A: Manifest, B: Manifest, X: Manifest] extends
      ((Rep[A] => Rep[X], Rep[B] => Rep[X]) => Rep[X]) {

    def isLeft: Rep[Boolean]

    def map[C: Manifest, D: Manifest](lmap: Rep[A] => Rep[C], rmap: Rep[B] => Rep[D])
        = EitherCPS[C, D, X](

      (l: Rep[C] => Rep[X], r: Rep[D] => Rep[X]) => this.apply(
        (a: Rep[A]) => l(lmap(a)),
        (b: Rep[B]) => r(rmap(b))
      ),

      this.isLeft
    )
  }

  /**
   * companion object, defining apply
   */
  object EitherCPS {

    def apply[A: Manifest, B: Manifest, X: Manifest](
      f: (Rep[A] => Rep[X], Rep[B] => Rep[X]) => Rep[X],
      isL: Rep[Boolean]
    ) = new EitherCPS[A, B, X] {
      def apply(left: Rep[A] => Rep[X], right: Rep[B] => Rep[X]) = f(left, right)
      def isLeft: Rep[Boolean] = isL
    }

    def LeftCPS[A: Manifest, B: Manifest, X: Manifest](a: Rep[A]) = EitherCPS(
      (l: Rep[A] => Rep[X], r: Rep[B] => Rep[X]) => l(a),
      unit(true)
    )

    def RightCPS[A: Manifest, B: Manifest, X: Manifest](b: Rep[B]) = EitherCPS(
      (l: Rep[A] => Rep[X], r: Rep[B] => Rep[X]) => r(b),
      unit(false)
    )
  }
}

