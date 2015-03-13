package lms.util

import lms._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait EitherProg extends EitherOps with PrimitiveOps with NumericOps {

  //map on left
  def eitherfoldleft(in: Rep[Int]): Rep[Either[Int, Int]] = {
    val s = left[Int, Int](in)
    s.map(x => x * unit(2), x => x * unit(3))
  }

  //map on right
  def eitherfoldright(in: Rep[Int]): Rep[Either[Int, Int]] = {
    val s = right[Int, Int](in)
    s.map(x => x * unit(2), x => x * unit(3))
  }
}

class EitherOpsSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testOption = {
    withOutFile(prefix + "either") {
      new EitherProg with EitherOpsExp with PrimitiveOpsExpOpt with NumericOpsExpOpt
        with StructOpsExpOptCommon with MyScalaCompile { self =>

        val codegen = new ScalaGenEitherOps with ScalaGenPrimitiveOps
          with ScalaGenNumericOps with lms.ScalaGenStructOps { val IR: self.type = self }

        codegen.emitSource(eitherfoldleft _, "eitherfoldleft", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcEitherfoldleft = compile(eitherfoldleft)
        scala.Console.println(testcEitherfoldleft(3))
        codegen.reset

        codegen.emitSource(eitherfoldright _, "eitherfoldright", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcEitherfoldright = compile(eitherfoldright)
        scala.Console.println(testcEitherfoldright(3))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "either")
  }
}
