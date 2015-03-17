package stagedparsec

import lms._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait StringReaderProg extends StringReaderOps with MiscOps {

  //print reader.first
  def testFirst(in: Rep[Array[Char]]): Rep[Char] = {
    val rdr: Rep[StringReader] = StringReader(in)
    rdr.first
  }

  def testAtEnd(in: Rep[Array[Char]], offset: Rep[Int]): Rep[Boolean] = {
    val rdr: Rep[StringReader] = StringReader(in, offset)
    rdr.atEnd
  }

  //compute rdr.rest and print first
  def testRest(in: Rep[Array[Char]]): Rep[Char] = {
    val rdr: Rep[StringReader] = StringReader(in)
    val rst = rdr.rest

    rst.first
  }

  def testIteration(in: Rep[Array[Char]]): Rep[Unit] = {
    val rdr = StringReader(in)
    rdr.foreach { c => println(c) }
  }
}

class StringReaderSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testStringReader = {
    withOutFile(prefix + "stringreader") {
      new StringReaderProg
        with StringReaderOpsExp
        with MiscOpsExp
        with MyScalaCompile { self =>

        val codegen = new ScalaGenStringReaderOps with ScalaGenMiscOps { val IR: self.type = self }

        codegen.emitSource(testFirst _, "testFirst", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFirst = compile(testFirst)
        scala.Console.println(testcFirst("hello".toArray))
        codegen.reset

        codegen.emitSource2(testAtEnd _, "testAtEnd", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcAtEnd = compile2(testAtEnd)
        scala.Console.println(testcAtEnd("hello".toArray, 0))
        scala.Console.println(testcAtEnd("hello".toArray, 6))
        codegen.reset

        codegen.emitSource(testRest _, "testRest", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcRest = compile(testRest)
        scala.Console.println(testcRest("hello".toArray))
        codegen.reset

        codegen.emitSource(testIteration _, "testIteration", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcIteration = compile(testIteration)
        testcIteration("hello".toArray)
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "stringreader")
  }
}
