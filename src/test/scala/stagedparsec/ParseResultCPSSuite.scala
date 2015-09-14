package stagedparsec

import scala.lms.common._
import scala.reflect.SourceContext
import scala.lms.internal.GenericCodegen

import lms._
import lms.util._
import lms.testutil.FileDiffSpec

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

/** stupid feature import*/
import scala.language.postfixOps

trait ParseResultCPSProg
    extends ParseResultCPS
    with OrderingOps
    with PrimitiveOps
    with NumericOps
    with StringReaderOps {

  import ParseResultCPS._

  /**
   * NOTE: we use `conditional` instead of the classic
   * if then else sugar, because there is either the virtualized
   * version (which required Reps for everything) or classic
   * version (no Reps anywhere)
   */

  /**
   * should generate code in which the reader should be DCE'd
   */
  def singleConditional(in: Rep[Array[Char]], flag: Rep[Boolean]): Rep[Option[Int]] = {
    val tmp = StringReader(in)
    val c = conditional(flag,
      Success(unit(1), tmp),
      Failure[Int](tmp.rest)
    )
    c.toOption
  }

  /**
   * should generate code in which the reader should be DCE'd
   */
  def nestedConditional(in: Rep[Array[Char]], i: Rep[Int]): Rep[Option[Int]] = {
    val tmp = StringReader(in)
    val c: ParseResultCPS[Int] = conditional(
      i <= unit(3),
      conditional(i >= unit(1), Success(i, tmp), Failure(tmp)),
      conditional(i >= unit(5), Success(i, tmp), Failure(tmp))
    )
    c.toOption
  }

  /**
   * should generate code where the notion of parseresult
   * has disappeard
   */
  def mapSuccess(in: Rep[Array[Char]], i: Rep[Int]): Rep[Option[Int]] = {
    val s = Success(i, StringReader(in))
    s.map(x => x * unit(2)).toOption
  }

  /**
   * should generate code where the notion of parseresult
   * has disappeard
   */
  def mapFailure(in: Rep[Array[Char]], i: Rep[Int]): Rep[Option[Int]] = {
    val s = Failure[Int](StringReader(in))
    s.map(x => x * unit(2)).toOption
  }

  /** code for in.rest must be generated in the else branch only */
  def mapConditional(in: Rep[Array[Char]], i: Rep[Int]): Rep[Option[Int]] = {
    val tmp = StringReader(in)
    val s = conditional(i <= unit(3), Success(i, tmp), Failure[Int](tmp.rest))
    s.map(_ * unit(2)).toOption
  }

  def mapConditional2(in: Rep[Array[Char]], i: Rep[Int]): Rep[Option[Int]] = {
    val tmp = StringReader(in)
    val s = conditional(i <= unit(3), Success(i, tmp), Success(i, tmp.rest))
    s.map(_ * unit(3)).toOption
  }

  def mapNestedConditional(in: Rep[Array[Char]], i: Rep[Int]): Rep[Option[Int]] = {
    val tmp = StringReader(in)
    val s: ParseResultCPS[Int] = conditional(
      i <= unit(3),
      conditional(i >= unit(1), Success(i, tmp), Failure(tmp)),
      Success(i * unit(2), tmp)
    )
    s.map(_ * unit(3)).toOption
  }

  /**
   * should generate code where only the nested parseresult is created
   */
  def flatMapSome(in: Rep[Array[Char]], i: Rep[Int]): Rep[Option[Int]] = {
    Success(i, StringReader(in)) flatMapWithNext { (x, rdr) =>
      Success(x * unit(2), rdr)
    } toOption
  }

  /**
   * should generate code where just a failure is created
   */
  def flatMapNone(in: Rep[Array[Char]], i: Rep[Int]): Rep[Option[Int]] = {
    Failure[Int](StringReader(in)) flatMapWithNext { (x, rdr) =>
      Success(x * unit(2), rdr)
    } toOption
  }

  def flatMapConditional(in: Rep[Array[Char]], i: Rep[Int]): Rep[Option[Int]] = {
    val tmp = StringReader(in)
    val s: ParseResultCPS[Int] = conditional(i <= unit(3), Success(i, tmp), Failure(tmp))
    s flatMapWithNext { (x, rdr) =>
      conditional(x >= unit(1), Success(x * unit(5), rdr), Success(x * unit(10), rdr.rest))
    } toOption
  }

  /**
   * resembles code from a sequencing parser combinator
   */
  def flatMapTilde(in: Rep[Array[Char]], idx: Rep[Int]): Rep[Option[Char]] = {
    val tmp = StringReader(in, idx)

    val h: ParseResultCPS[Char] = conditional(
      tmp.atEnd,
      Failure(tmp),
      conditional(
        tmp.first == unit('h'),
        Success(tmp.first, tmp.rest),
        Failure(tmp)
      )
    )

    val he: ParseResultCPS[Char] = h flatMapWithNext { (c, rdr) =>
      conditional(
        rdr.atEnd,
        Failure(rdr),
        conditional(
          rdr.first == unit('e'),
          Success(rdr.first, rdr.rest),
          Failure(tmp)
        )
      )
    }

    he.toOption
  }

  /**
   * code similar to an alternating combinator in parser combinators,
   * but manually inlined
   * IMPORTANT: the code for `second` should only be generated once,
   * i.e. there should be a join point after `first`
   */
  def orElseAlternation(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val tmp = StringReader(in)
    val first: ParseResultCPS[Char] = conditional(
      tmp.atEnd,
      Failure(tmp),
      conditional(
        tmp.first == unit('a'),
        Success(tmp.first, tmp.rest),
        Failure(tmp)
      )
    )

    val second: ParseResultCPS[Char] = conditional(
      tmp.atEnd,
      Failure(tmp),
      conditional(
        tmp.first == unit('b'),
        Success(tmp.first, tmp.rest),
        Failure(tmp)
      )
    )

    (first orElse second) toOption
  }

  /**
   * code similar to an alternating combinator in parser combinators,
   * followed by another parser, but manually inlined
   * IMPORTANT: the code for `second` should only be generated once,
   * i.e. there should be a join point after `first`
   * The code in the `flatMapWithNext` call should also be generated only once
   */
  def orElseSeq(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val tmp = StringReader(in)
    val first: ParseResultCPS[Char] = conditional(
      tmp.atEnd,
      Failure(tmp),
      conditional(
        tmp.first == unit('a'),
        Success(tmp.first, tmp.rest),
        Failure(tmp)
      )
    )

    val second: ParseResultCPS[Char] = conditional(
      tmp.atEnd,
      Failure(tmp),
      conditional(
        tmp.first == unit('b'),
        Success(tmp.first, tmp.rest),
        Failure(tmp)
      )
    )

    (first orElse second) flatMapWithNext { (_, rdr) =>
      conditional[Char](
        rdr.atEnd,
        Failure(rdr),
        conditional(
          rdr.first == unit('c'),
          Success(rdr.first, rdr.rest),
          Failure(rdr)
        )
      )
    } toOption
  }

/*
  def filtersome(in: Rep[Int]): Rep[Option[Int]] = {
    val s: Rep[Option[Int]] = mkSome(in)
    s.filter(x => x == unit(3)).toOption
  }

  def filternone(in: Rep[Int]): Rep[Option[Int]] = {
    val s = mkNone[Int]
    s.filter(x => x == unit(2)).toOption
  }
*/
}

class ParseResultCPSSuite extends FileDiffSpec {

  val prefix = "test-out/"

  def `ParseResultCPS generates code with no diff` = {
    withOutFile(prefix + "parseresultcps") {
      new ParseResultCPSProg
          with ParseResultCPSExpOpt
          with OrderingOpsExpOpt
          with PrimitiveOpsExpOpt
          with NumericOpsExpOpt
          with StringReaderOpsExpOpt
          /** this trait should be mixed in higher up */ with ArrayOpsExp
          /** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new ScalaGenBase
          with ScalaGenParseResultCPS
          with ScalaGenOrderingOps
          with ScalaGenEqual
          with ScalaGenVariables
          with ScalaGenPrimitiveOps
          with ScalaGenNumericOps
          with ScalaGenStringReaderOps { val IR: self.type = self }

        codegen.emitSource2(singleConditional _, "singleConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcSingleConditional = compile2(singleConditional)
        scala.Console.println(testcSingleConditional("".toArray, true))
        codegen.reset

        codegen.emitSource2(nestedConditional _, "nestedConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedConditional = compile2(nestedConditional)
        scala.Console.println(testcNestedConditional("".toArray, 5))
        codegen.reset

        codegen.emitSource2(mapSuccess _, "mapSuccess", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapSuccess = compile2(mapSuccess)
        scala.Console.println(testcMapSuccess("".toArray, 5))
        codegen.reset

        codegen.emitSource2(mapFailure _, "mapFailure", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapFailure = compile2(mapFailure)
        scala.Console.println(testcMapFailure("".toArray, 5))
        codegen.reset

        codegen.emitSource2(mapConditional _, "mapConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapConditional = compile2(mapConditional)
        scala.Console.println(testcMapConditional("h".toArray, 5))
        scala.Console.println(testcMapConditional("h".toArray, 3))
        codegen.reset

        codegen.emitSource2(mapConditional2 _, "mapConditional2", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapConditional2 = compile2(mapConditional2)
        scala.Console.println(testcMapConditional2("h".toArray, 5))
        scala.Console.println(testcMapConditional2("h".toArray, 3))
        codegen.reset

        codegen.emitSource2(mapNestedConditional _, "mapNestedConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapNestedConditional = compile2(mapNestedConditional)
        scala.Console.println(testcMapNestedConditional("h".toArray, 5))
        scala.Console.println(testcMapNestedConditional("h".toArray, 3))
        codegen.reset

        codegen.emitSource2(flatMapSome _, "flatMapSome", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapSome = compile2(flatMapSome)
        scala.Console.println(testcFlatMapSome("h".toArray, 5))
        codegen.reset

        codegen.emitSource2(flatMapNone _, "flatMapNone", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapNone = compile2(flatMapNone)
        scala.Console.println(testcFlatMapNone("h".toArray, 5))
        codegen.reset

        codegen.emitSource2(flatMapConditional _, "flatMapConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapConditional = compile2(flatMapConditional)
        scala.Console.println(testcFlatMapConditional("hello".toArray, 5))
        scala.Console.println(testcFlatMapConditional("hello".toArray, 3))
        scala.Console.println(testcFlatMapConditional("hello".toArray, 0))
        codegen.reset

        codegen.emitSource2(flatMapTilde _, "flatMapTilde", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapTilde = compile2(flatMapTilde)
        scala.Console.println(testcFlatMapTilde("hello".toArray, 2))
        scala.Console.println(testcFlatMapTilde("hello".toArray, 0))
        codegen.reset

        codegen.emitSource(orElseAlternation _, "orElseAlternation", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOrElseAlternation = compile(orElseAlternation)
        scala.Console.println(testcOrElseAlternation("a".toArray))
        scala.Console.println(testcOrElseAlternation("b".toArray))
        codegen.reset

        codegen.emitSource(orElseSeq _, "orElseSeq", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOrElseSeq = compile(orElseSeq)
        scala.Console.println(testcOrElseSeq("ac".toArray))
        scala.Console.println(testcOrElseSeq("bc".toArray))
        scala.Console.println(testcOrElseSeq("ab".toArray))
        scala.Console.println(testcOrElseSeq("c".toArray))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "parseresultcps")
  }
}
