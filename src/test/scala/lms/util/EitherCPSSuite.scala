package lms.util

import scala.lms.common._
import scala.reflect.SourceContext
import scala.lms.internal.GenericCodegen
import lms._
import lms.util._

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait EitherCPSProg
    extends EitherCPSOps
    with OrderingOps {

  import EitherCPS._

  /**
   * just a single conditional expression
   */
  def singleConditional(in: Rep[Int]): Rep[Int] = {
    var res = unit(0)

    /** gets picked up for both sides,
     * lucky it makes sense here*/
    implicit val lValue: Option[Var[Int]] = None

    val c = if (in <= unit(3)) mkLeft[Int, Int](unit(2))
            else mkRight[Int, Int](unit(4))

    c.apply(l => res = l, r => res = r)
    res
  }

  /**
   * just a single conditional expression
   * This time we make the implicit hold a value
   */
  def singleConditionalBis(in: Rep[Int]): Rep[Int] = {
    var res = unit(0)

    /** gets picked up for both sides,
     * lucky it makes sense here*/
    implicit val lValue: Option[Var[Int]] = Some(res)

    val c = if (in <= unit(3)) mkLeft[Int, Int](unit(2))
            else mkRight[Int, Int](unit(4))

    c.apply(l => res = l, r => res = r)
    res
  }

  /**
   * Let's nest the conditional in
   */
  def nestedConditional(in: Rep[Int]): Rep[Int] = {
    var res = unit(0)

    /** gets picked up for both sides,
     * lucky it makes sense here*/
    implicit val lValue: Option[Var[Int]] = None

    val c = if (in <= unit(3)) {
      if (in >= unit(1)) mkLeft[Int, Int](unit(2))
      else mkRight[Int, Int](unit(3))

    } else mkRight[Int, Int](unit(4))

    c.apply(l => res = l, r => res = r)
    res
  }

  /**
   * Let's nest the conditional in again,
   * this time we give a default value
   * for the implicit
   */
  def nestedConditionalBis(in: Rep[Int]): Rep[Int] = {
    var res = unit(0)

    /** gets picked up for both sides,
     * lucky it makes sense here */
    implicit val lValue: Option[Var[Int]] = Some(res)

    val c = if (in <= unit(3)) {
      if (in >= unit(1)) mkLeft[Int, Int](unit(2))
      else mkRight[Int, Int](unit(3))

    } else mkRight[Int, Int](unit(4))

    c.apply(l => res = l, r => res = r)
    res
  }


}

class EitherCPSSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testEitherCPS = {
    withOutFile(prefix + "eithercps") {
      new EitherCPSProg
          with EitherCPSOpsExp
          with OrderingOpsExp
          /** this trait should be mixed in higher up */ with ArrayOpsExp
          /** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new ScalaGenBase
            with ScalaGenIfThenElse
            with ScalaGenBooleanOps
            with ScalaGenOrderingOps
            with ScalaGenEqual
            with ScalaGenVariables { val IR: self.type = self }

        codegen.emitSource(singleConditional _, "singleConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcSingleConditional = compile(singleConditional)
        scala.Console.println(testcSingleConditional(5))
        codegen.reset

        codegen.emitSource(singleConditionalBis _, "singleConditionalBis", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcSingleConditionalBis = compile(singleConditionalBis)
        scala.Console.println(testcSingleConditionalBis(5))
        codegen.reset

        codegen.emitSource(nestedConditional _, "nestedConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedConditional = compile(nestedConditional)
        scala.Console.println(testcNestedConditional(5))
        codegen.reset

        codegen.emitSource(nestedConditionalBis _, "nestedConditionalBis", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedConditionalBis = compile(nestedConditionalBis)
        scala.Console.println(testcNestedConditionalBis(5))
        codegen.reset



      }
    }

    assertFileEqualsCheck(prefix + "eithercps")
  }
}

