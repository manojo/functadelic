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
  abstract class EitherCPS[A: Manifest, B: Manifest] { self =>

    def apply[X: Manifest](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]): Rep[X]

    def isLeft: Rep[Boolean]

    def map[C: Manifest, D: Manifest](lmap: Rep[A] => Rep[C], rmap: Rep[B] => Rep[D])
      = new EitherCPS[C, D] {

        def isLeft = self.isLeft

        def apply[X: Manifest](lf: Rep[C] => Rep[X], rf: Rep[D] => Rep[X]) = self.apply(
          a => lf(lmap(a)),
          b => rf(rmap(b))
        )
      }
  }

  /**
   * companion object, defining apply
   */
  object EitherCPS {

    def LeftCPS[A: Manifest, B: Manifest](a: Rep[A]) = new EitherCPS[A, B] {

      def apply[X: Manifest](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]) =
        lf(a)

      def isLeft = unit(true)
    }

    def RightCPS[A: Manifest, B: Manifest](b: Rep[B]) = new EitherCPS[A, B] {

      def apply[X: Manifest](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]) =
        rf(b)

      def isLeft = unit(false)
    }
  }
}
