package stagedparsec

import lms._
import lms.util._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream


trait ParseResultProg
    extends ParseResultOps
    with StringReaderOps
    with OptionOps {

  /**
   * a simple conditional expression that
   * yields a parse result on both sides
   */
  def conditional(in: Rep[Array[Char]], flag: Rep[Boolean]): Rep[Option[Int]] = {
    val tmp = StringReader(in)
    val presult = if (flag) Success[Int](unit(1), tmp) else Failure[Int](tmp)
    if (presult.isEmpty) none[Int]() else Some(presult.get)
  }

  /**
   * conditional expression followed by `map`
   */
  def conditionalMap(in: Rep[Array[Char]], flag: Rep[Boolean]): Rep[Option[Int]] = {
    val tmp = StringReader(in)
    val presult = if (flag) Success[Int](unit(1), tmp) else Failure[Int](tmp)
    val mapped = presult.map(_ * unit(2))
    if (mapped.isEmpty) none[Int]() else Some(mapped.get)
  }

  /**
   * conditional expression followed by hand-written `map`
   */
  def conditionalMapHand(in: Rep[Array[Char]], flag: Rep[Boolean]): Rep[Option[Int]] = {
    val tmp = StringReader(in)
    val presult = if (flag) Success[Int](unit(1), tmp) else Failure[Int](tmp)
    val mapped = if (presult.isEmpty) Failure[Int](tmp)
                 else Success(presult.get * unit(2), tmp)

    if (mapped.isEmpty) none[Int]() else Some(mapped.get)
  }

}

class ParseResultSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testParseResults = {
    withOutFile(prefix + "parse-result") {
      /**
       * Attention: Need to mix in Fat versions of StructOps as well as IfthenElse
       * for optimisations on FatIfs and so on.
       * Note: We are also using our own version of IfThenElseGenFat
       * to generate variables instead of tuples and boundary ends
       * of conditional expressions.
       */
      new ParseResultProg
          with ParseResultOpsExp
          with StringReaderOpsExp
          with OptionOpsExp
          with StructOpsFatExpOptCommon
          with MyScalaCompile { self =>

        val codegen = new ScalaGenParseResultOps
            with ScalaGenStringReaderOps
            with ScalaGenOptionOps
            with ScalaGenFatStructOps
            with MyScalaGenIfThenElseFat {
          val IR: self.type = self
        }

        codegen.emitSource2(conditional _, "conditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcConditional = compile2(conditional)
        scala.Console.println(testcConditional("".toArray, true))
        scala.Console.println(testcConditional("".toArray, false))
        codegen.reset

        codegen.emitSource2(conditionalMap _, "conditionalMap", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcConditionalMap = compile2(conditionalMap)
        scala.Console.println(testcConditionalMap("".toArray, true))
        scala.Console.println(testcConditionalMap("".toArray, false))
        codegen.reset

        codegen.emitSource2(
          conditionalMapHand _,
          "conditionalMapHand",
          new java.io.PrintWriter(System.out)
        )
        codegen.reset

        val testcConditionalMapHand = compile2(conditionalMapHand)
        scala.Console.println(testcConditionalMapHand("".toArray, true))
        scala.Console.println(testcConditionalMapHand("".toArray, false))
        codegen.reset

      }
      assertFileEqualsCheck(prefix + "parse-result")
    }
  }
}
