package stagedparsec

import lms._
import scala.lms.common._
import scala.lms.internal.Effects

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait RecParsersProg
    extends CharParsers
    with Functions {

  import Parser._

  /**
   * recursive digit parser
   * num = digit2int ~ num
   */
  def recNumber(in: Rep[Array[Char]]): Rep[Option[Int]] = {

    lazy val parser: Parser[Int] = rec(
      ((digit2Int ~ parser) map {
        x: Rep[(Int, Int)] => x._1 + x._2
      }) | digit2Int
    )

    phrase(parser, StringReader(in))

  }
/*
  // expr = term ~ rep("+" ~ term)
  // term = factor ~ rep("*" ~ factor)
  // factor = wholeNumber | "("~ expr ~")"

  def recExpr: Parser[Int] = {
    def expr: Parser[Int] = rec("wow",
      (term ~ repFold(accept(unit('+')) ~> term)(unit(0), (x: Rep[Int], y: Rep[Int]) => x + y))
        ^^ { x => x._1 + x._2 }
    )

    def term: Parser[Int] = (
      factor ~ repFold(accept(unit('*')) ~> factor)(unit(1), (x: Rep[Int], y: Rep[Int]) => x * y)
    ) ^^ { x => x._1 * x._2 }

    def factor = wholeNumber | accept(unit('(')) ~> expr <~ accept(unit(')'))

    expr
  }
*/
}


class RecParsersSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testRecParsers = {
    withOutFile(prefix + "rec-parser") {
      new RecParsersProg
          with CharParsersExp
          with IfThenElseExpOpt
          with StructExpOptCommon
          /** this trait should be mixed in higher up */ with ArrayOpsExp
          /** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new ScalaGenCharParsers
            with ScalaGenStruct
            with ScalaGenIfThenElse {
          val IR: self.type = self
        }

        codegen.emitSource(recNumber _, "recNumber", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcRecNumber = compile(recNumber)
        scala.Console.println(testcRecNumber("12345".toArray))
        codegen.reset
      }
      assertFileEqualsCheck(prefix + "rec-parser")
    }
  }
}
