package lms.util

import lms._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait EitherProg extends EitherOps with PrimitiveOps with NumericOps {

  //map on left
  def eithermapleft(in: Rep[Int]): Rep[Either[Int, Int]] = {
    val s = left[Int, Int](in)
    s.map(x => x * unit(2), x => x * unit(3))
  }

  //map on right
  def eithermapright(in: Rep[Int]): Rep[Either[Int, Int]] = {
    val s = right[Int, Int](in)
    s.map(x => x * unit(2), x => x * unit(3))
  }
}

class EitherOpsSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testOption = {
    withOutFile(prefix + "either") {
      new EitherProg
          with EitherOpsExpOpt
          with PrimitiveOpsExpOpt
          with NumericOpsExpOpt
          with StructExpOptCommon
          with MyScalaCompile { self =>

        val codegen = new ScalaGenEitherOps with ScalaGenPrimitiveOps
          with ScalaGenNumericOps with ScalaGenStruct { val IR: self.type = self }

        codegen.emitSource(eithermapleft _, "eithermapleft", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcEithermapleft = compile(eithermapleft)
        scala.Console.println(testcEithermapleft(3))
        codegen.reset

        codegen.emitSource(eithermapright _, "eithermapright", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcEithermapright = compile(eithermapright)
        scala.Console.println(testcEithermapright(3))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "either")
  }
}
