package stagedparsec

import lms._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream


trait CharParsersProg extends CharParsers {

  import Parser._

  /**
   * simple acceptIf filter
   */
  def acceptIf(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser = acceptIf(x => x == unit('h'))
    phrase(parser, StringReader(in))
  }

  /**
   * simple acceptIf filter, special character
   * Note: this will codegen correctly if used in conjuntion
   * with ScalaGenCharOps.
   */
  def acceptIfSpec(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser = acceptIf(x => x == unit('\\'))
    phrase(parser, StringReader(in))
  }

  /**
   * accept function: generates the exact same code as acceptIf
   */
  def accept(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser = accept(unit('h'))
    phrase(parser, StringReader(in))
  }

  /**
   * parsing a single letter
   */
  def letterParser(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser = letter
    phrase(parser, StringReader(in))
  }

  /**
   * parsing a single digit
   */
  def digitParser(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser = digit
    phrase(parser, StringReader(in))
  }

  //two letters
  def twoLetters(in: Rep[Array[Char]]): Rep[Option[(Char, Char)]] = {
    val parser = letter ~ letter
    phrase(parser, StringReader(in))
  }

  /*
  //ignoring left result
  def test6(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser = letter ~> letter
    phrase(parser, StringReader(in))
  }

  //ignoring right result
  def test7(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser = letter <~ letter
    phrase(parser, StringReader(in))
  }

  //digit to int
  def test8(in: Rep[Array[Char]]): Rep[Option[Int]] = {
    val parser = digit2Int
    phrase(parser, StringReader(in))
  }

  //or
  def test9(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser = letter | digit
    phrase(parser, StringReader(in))
  }

  //or2: testing that or creates functions
  def testOr2(in: Rep[Array[Char]]): Rep[Option[(Char, Char)]] = {
    val parser =
      (accept(unit('h')) ~ accept(unit('e'))) |
        (accept(unit('1')) ~ accept(unit('2')))

    phrase(parser, StringReader(in))
  }

  //or3: (a | b) ~ c
  def testOr3(in: Rep[Array[Char]]): Rep[Option[((Char, Char), Char)]] = {
    val parser =
      ((accept(unit('h')) ~ accept(unit('e'))) |
        (accept(unit('1')) ~ accept(unit('2')))
      ) ~ accept(unit('3'))

    phrase(parser, StringReader(in))
  }

  def testrep1Fold(in: Rep[Array[Char]]): Rep[Option[Int]] = {
    val parser = rep1Fold(digit2Int, digit2Int)((x,y) => x + y)
    phrase(parser, StringReader(in))
  }

  //rep
  def test10(in: Rep[Array[Char]]): Rep[Option[String]] = {
    val parser = rep(letter) ^^ { x: Rep[List[Char]] => x.mkString }
    phrase(parser, StringReader(in))
  }

  //repFold
  def test11(in: Rep[Array[Char]]): Rep[Option[Int]] = {
    val parser =
      repFold(digit2Int)(unit(0), (x: Rep[Int], y: Rep[Int]) => x + y)
    phrase(parser, StringReader(in))
  }

  def test12(in: Rep[Array[Char]]): Rep[Option[(Char,Char)]] = {
    val parser =
      repFold(digit ~ digit)(make_tuple2(unit('a'), unit('a')), (x: Rep[(Char, Char)], y: Rep[(Char, Char)]) => y)
    phrase(parser, StringReader(in))
  }

  //cond
  def testCond(in: Rep[Array[Char]], n: Rep[Int]): Rep[Option[Char]] = {
    val parser: Parser[Char] =
      if (n < unit(3)) accept(unit('b'))
      else accept(unit('c'))
    phrase(parser, StringReader(in))
  }

  //bind
  def testBind(in: Rep[Array[Char]]): Rep[Option[String]] = {
    val parser = letter >> { x: Rep[Char] =>
      if (x == unit('a')) accept(unit('b')) ^^ { y: Rep[Char] => x + unit(", ") + y }
      else accept(unit('d')) ^^ { y: Rep[Char] => x + unit(", ") + y }
    }

    phrase(parser, StringReader(in))
  }
  */
}

class CharParsersSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testCharParsers = {
    withOutFile(prefix + "char-parser") {
      /**
       * Attention: Need to mix in Fat versions of StructOps as well as IfthenElse
       * for optimisations on FatIfs and so on.
       * Note: We are also using our own version of IfThenElseGenFat
       * to generate variables instead of tuples and boundary ends
       * of conditional expressions.
       */
      new CharParsersProg
          with CharParsersExp
          with IfThenElseExpOpt
          with StructOpsFatExpOptCommon
          with MyScalaCompile { self =>

        val codegen = new ScalaGenCharParsers with ScalaGenFatStructOps with MyScalaGenIfThenElseFat {
          val IR: self.type = self
        }

        codegen.emitSource(acceptIf _, "acceptIf", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcAcceptIf = compile(acceptIf)
        scala.Console.println(testcAcceptIf("hello".toArray))
        scala.Console.println(testcAcceptIf("ello".toArray))
        codegen.reset

        codegen.emitSource(acceptIfSpec _, "acceptIfSpec", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcAcceptIfSpec = compile(acceptIfSpec)
        scala.Console.println(testcAcceptIfSpec("\\ello".toArray))
        scala.Console.println(testcAcceptIfSpec("hello".toArray))
        codegen.reset

        codegen.emitSource(letterParser _, "letterParser", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcLetterParser = compile(letterParser)
        scala.Console.println(testcLetterParser("hello".toArray))
        scala.Console.println(testcLetterParser("1".toArray))
        codegen.reset

        codegen.emitSource(digitParser _, "digitParser", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcDigitParser = compile(digitParser)
        scala.Console.println(testcDigitParser("hello".toArray))
        scala.Console.println(testcDigitParser("1".toArray))
        codegen.reset

        codegen.emitSource(twoLetters _, "twoLetters", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcTwoLetters = compile(twoLetters)
        scala.Console.println(testcTwoLetters("hello".toArray)) //succeeding a ~ b
        scala.Console.println(testcTwoLetters("1ello".toArray)) //failing left
        scala.Console.println(testcTwoLetters("h2llo".toArray)) //failing right
        codegen.reset

/*
        codegen.emitSource(test5 _, "test5", new java.io.PrintWriter(System.out))
        codegen.reset

        val testc5 = compile(test5)
        scala.Console.println(testc5("hello".toArray)) //succeeding a ~ b
        scala.Console.println(testc5("1ello".toArray)) //failing left
        scala.Console.println(testc5("h2llo".toArray)) //failing right
        codegen.reset

        codegen.emitSource(test6 _, "test6", new java.io.PrintWriter(System.out))
        codegen.reset

        val testc6 = compile(test6)
        scala.Console.println(testc6("hello".toArray))
        codegen.reset

        codegen.emitSource(test7 _, "test7", new java.io.PrintWriter(System.out))
        codegen.reset

        val testc7 = compile(test7)
        scala.Console.println(testc7("hello".toArray))
        codegen.reset

        //digit2Int
        codegen.emitSource(test8 _, "test8", new java.io.PrintWriter(System.out))
        codegen.reset

        val testc8 = compile(test8)
        scala.Console.println(testc8("1ello".toArray))
        scala.Console.println(testc8("hello".toArray))
        codegen.reset

        codegen.emitSource(test9 _, "test9", new java.io.PrintWriter(System.out))
        codegen.reset

        val testc9 = compile(test9)
        scala.Console.println(testc9("hello".toArray))
        scala.Console.println(testc9("12".toArray))
        scala.Console.println(testc9(":".toArray))
        codegen.reset

        codegen.emitSource(testOr2 _, "testOr2", new java.io.PrintWriter(System.out))
        codegen.emitDataStructures(new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOr2 = compile(testOr2)
        scala.Console.println(testcOr2("hello".toArray))
        scala.Console.println(testcOr2("12".toArray))
        scala.Console.println(testcOr2(":".toArray)) //fail case
        scala.Console.println(testcOr2("h1".toArray)) //fail case
        scala.Console.println(testcOr2("1d".toArray)) //fail case
        codegen.reset

        codegen.emitSource(testrep1Fold _, "testrep1Fold", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcRep1Fold = compile(testrep1Fold)
        scala.Console.println(testcRep1Fold("12345".toArray))
        codegen.reset

        codegen.emitSource(test10 _, "test10", new java.io.PrintWriter(System.out))
        codegen.reset

        val testc10 = compile(test10)
        scala.Console.println(testc10("hello21".toArray))
        codegen.reset

        codegen.emitSource(test11 _, "test11", new java.io.PrintWriter(System.out))
        codegen.reset

        val testc11 = compile(test11)
        scala.Console.println(testc11("12345".toArray))
        scala.Console.println(testc11("asd".toArray))
        scala.Console.println(testc11("".toArray))
        codegen.reset

        codegen.emitSource(test12 _, "test12", new java.io.PrintWriter(System.out))
        codegen.reset

        val testc12 = compile(test12)
        scala.Console.println(testc12("1234".toArray))
        codegen.reset

        val printWriter = new java.io.PrintWriter(System.out)
        codegen.emitSource2(testCond _, "testCond", printWriter)
        codegen.emitDataStructures(printWriter)
        codegen.reset

        val source = new StringWriter
        codegen.emitDataStructures(new PrintWriter(source))
        val testcCond = compile2s(testCond, source)
        scala.Console.println(testcCond("b".toArray, 2))
        scala.Console.println(testcCond("c".toArray, 6))
        codegen.reset

        codegen.emitSource(testBind _, "testBind", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcBind = compile(testBind)
        scala.Console.println(testcBind("ab".toArray)) //successful
        scala.Console.println(testcBind("ac".toArray)) //fail
        scala.Console.println(testcBind("cd".toArray)) //successful
        scala.Console.println(testcBind("ca".toArray)) //fail
        codegen.reset
*/
      }
      assertFileEqualsCheck(prefix + "char-parser")
    }
  }
}

  /*
  def testOr {
    withOutFile(prefix + "or-parser") {
      new CharParsersProg with MyScalaOpsPkgExp with CharOpsExp
        with MyIfThenElseExpOpt with StructOpsFatExpOptCommon
        with ParseResultOpsExp with FunctionsExp with OptionOpsExp
        with StringStructOpsExp with BarrierOpsExp with StringReaderOpsExp with MyScalaCompile { self =>

        val codegen = new MyScalaCodeGenPkg with ScalaGenCharOps with ScalaGenParseResultOps
          with ScalaGenFatStructOps with ScalaGenFunctions with ScalaGenStringStructOps
          with ScalaGenOptionOps with ScalaGenBarrierOps with ScalaGenIfThenElseFat with ScalaGenReaderOps {

          val IR: self.type = self
        }

        codegen.emitSource(testOr2 _, "testOr2", new java.io.PrintWriter(System.out))
        codegen.emitDataStructures(new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOr2 = compile(testOr2)
        scala.Console.println(testcOr2("hello".toArray))
        scala.Console.println(testcOr2("12".toArray))
        scala.Console.println(testcOr2(":".toArray)) //fail case
        scala.Console.println(testcOr2("h1".toArray)) //fail case
        scala.Console.println(testcOr2("1d".toArray)) //fail case
        codegen.reset

        codegen.emitSource(testOr3 _, "testOr3", new java.io.PrintWriter(System.out))
        codegen.emitDataStructures(new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOr3 = compile(testOr3)
        scala.Console.println(testcOr3("he3lo".toArray))
        scala.Console.println(testcOr3("123".toArray))
        scala.Console.println(testcOr3(":".toArray)) //fail case
        scala.Console.println(testcOr3("he1".toArray)) //fail case
        scala.Console.println(testcOr3("12d".toArray)) //fail case
        codegen.reset

      }
    }
    assertFileEqualsCheck(prefix + "or-parser")
  }
}*/
