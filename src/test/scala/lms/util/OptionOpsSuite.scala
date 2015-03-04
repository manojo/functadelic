package lms.util

import lms._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait OptionProg extends OptionOps with PrimitiveOps with NumericOps {

  //map
  def optionmapsome(in: Rep[Int]): Rep[Option[Int]] = {
    val s: Rep[Option[Int]] = Some(in)
    s.map(x => x * unit(2))
  }

  //map on None
  def optionmapnone(in: Rep[Int]): Rep[Option[Int]] = {
    //option[rep[int]] is implicitly converted to rep[option[int]]
    val s = None.asInstanceOf[Option[Rep[Int]]]
    s.map(x => x * unit(2))
  }

  //flatMap
  def optionflatmapsome(in: Rep[Int]): Rep[Option[Int]] = {
    val s: Rep[Option[Int]] = Some(in)
    s.flatMap(x => Some(x * unit(2)))
  }

  //flatMap on None
  def optionflatmapnone(in: Rep[Int]): Rep[Option[Int]] = {
    val s = None.asInstanceOf[Option[Rep[Int]]]
    s.flatMap(x => Some(x * unit(2)))
  }

  //filter
  def optionfiltersome(in: Rep[Int]): Rep[Option[Int]] = {
    val s: Rep[Option[Int]] = Some(in)
    s.filter { x: Rep[Int] => x == unit(3) }
  }

  //filter on None
  def optionfilternone(in: Rep[Int]): Rep[Option[Int]] = {
    val s = None.asInstanceOf[Option[Rep[Int]]]
    s.filter(x => x == unit(2))
  }
}

class OptionOpsSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testOption = {
    withOutFile(prefix + "option") {
      new OptionProg with OptionOpsExp with PrimitiveOpsExpOpt with NumericOpsExpOpt
        with StructOpsExpOptCommon with MyScalaCompile { self =>

        val codegen = new ScalaGenOptionOps with ScalaGenPrimitiveOps
          with ScalaGenNumericOps with lms.ScalaGenStructOps { val IR: self.type = self }

        codegen.emitSource(optionmapsome _, "optionmapsome", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOptionmapsome = compile(optionmapsome)
        scala.Console.println(testcOptionmapsome(3))
        codegen.reset

        codegen.emitSource(optionmapnone _, "optionmapnone", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOptionmapnone = compile(optionmapnone)
        scala.Console.println(testcOptionmapnone(3))
        codegen.reset

        codegen.emitSource(optionflatmapsome _, "optionflatmapsome", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOptionflatmapsome = compile(optionflatmapsome)
        scala.Console.println(testcOptionflatmapsome(3))
        codegen.reset

        codegen.emitSource(optionflatmapnone _, "optionflatmapnone", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOptionflatmapnone = compile(optionflatmapnone)
        scala.Console.println(testcOptionflatmapnone(3))
        codegen.reset

        codegen.emitSource(optionfiltersome _, "optionfiltersome", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOptionfiltersome = compile(optionfiltersome)
        scala.Console.println(testcOptionfiltersome(3))
        scala.Console.println(testcOptionfiltersome(2))
        codegen.reset

        codegen.emitSource(optionfilternone _, "optionfilternone", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOptionfilternone = compile(optionfilternone)
        scala.Console.println(testcOptionfilternone(3))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "option")
  }
}
