package lms.util

import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import scala.virtualization.lms.internal.GenericCodegen
import lms._

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait OptionCPSProg
    extends OptionCPSOps
    with OptionOps
    with OrderingOps {

  import OptionCPS._

  def singleConditional(in: Rep[Int]): Rep[Option[Int]] = {
    var res = none[Int]()
    val c = if (in <= unit(3)) mkSome(in) else mkNone[Int]
    c.apply((_: Rep[Unit]) => unit(()), x => res = make_opt(scala.Some(x)))
    res
  }

  def nestedConditional(in: Rep[Int]): Rep[Option[Int]] = {
    var res = none[Int]()

    val c =
      if (in <= unit(3)) {
        if (in >= unit(1)) mkSome(in) else mkNone[Int]
      } else {
        if (in >= unit(5)) mkSome(in) else mkNone[Int]
      }

    c.apply((_: Rep[Unit]) => unit(()), x => res = make_opt(scala.Some(x)))
    res
  }

  /**
   * The same conditional expression twice
   * to make sure that no optimization collapses the conditional
   */
  def nestedConditional2(in: Rep[Int]): Rep[Option[Int]] = {
    var res = none[Int]()

    val c =
      if (in == unit(3)) mkSome(in)
      else if (in == unit(3)) mkSome(in)
      else mkNone[Int]

    c.apply((_: Rep[Unit]) => unit(()), x => res = make_opt(scala.Some(x)))
    res
  }
}

class OptionCPSSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testOptionCPS = {
    withOutFile(prefix + "optioncps") {
      new OptionCPSProg
          with OptionCPSExp
          with OrderingOpsExp
          with OptionOpsExp
          with MyScalaCompile { self =>

        val codegen = new ScalaGenBase
            with ScalaGenIfThenElse
            with ScalaGenBooleanOps
            with ScalaGenOrderingOps
            with ScalaGenEqual
            with ScalaGenVariables
            with ScalaGenOptionOps { val IR: self.type = self }

        codegen.emitSource(singleConditional _, "singleConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcSingleConditional = compile(singleConditional)
        scala.Console.println(testcSingleConditional(5))
        codegen.reset

        codegen.emitSource(nestedConditional _, "nestedConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedConditional = compile(nestedConditional)
        scala.Console.println(testcNestedConditional(5))
        codegen.reset

        codegen.emitSource(nestedConditional2 _, "nestedConditional2", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedConditional2 = compile(nestedConditional2)
        scala.Console.println(testcNestedConditional2(5))
        codegen.reset


      }
    }

    assertFileEqualsCheck(prefix + "optioncps")
  }
}
