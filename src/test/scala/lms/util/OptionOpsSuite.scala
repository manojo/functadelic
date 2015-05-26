package lms.util

import lms._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait OptionProg
    extends OptionOps
    with PrimitiveOps
    with NumericOps
    with OrderingOps
    with ArrayOps {

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

  /**
   * conditional expressions on Option
   * If we just use StructExp, the conditional
   * will be split for each field. This is not always desirable
   * Instead we just want the boxing to be avoided,
   * and have a single conditional expression
   * FatIfs do this for us.
   */
  def optionConditional(in: Rep[Int]): Rep[Option[Int]] = {
    if (in == unit(3)) None.asInstanceOf[Option[Rep[Int]]]
    else Some(in)
  }

  /**
   * extended ifThenElse
   */
  def optionConditionalBis(in: Rep[Int]): Rep[Option[Int]] = {
    if (in == unit(3)) Some(in)
    else if (in == unit(3)) Some(in)
    else None.asInstanceOf[Option[Rep[Int]]]
  }

  /**
   * conditional on value followed by
   * conditional on the option itself expression
   * looks like the code from an alternation combinator
   * in parsers
   */
  def optionNestedConditional(in: Rep[Int]): Rep[Int] = {
/*
    val first =
      if (in == unit(3)) Some(in)
      else None.asInstanceOf[Option[Rep[Int]]]

    if (first.isDefined) first
    else if (in == unit(3)) Some(in)
    else None.asInstanceOf[Option[Rep[Int]]]


    if (in == unit(3)) {
      val bla = Some(in)
      if (bla.isDefined) bla
      else if (in == unit(3)) Some(in)
      else None
    } else {
      val bla = None
      if (bla.isDefined) bla
      else if (in == unit(3)) Some(in)
      else None
    }
*/
    if (in == unit(3)) {
      unit(2)//Some(in)
    } else {
      if (in == unit(3)) unit(2) //Some(in)
      else unit(0)// None.asInstanceOf[Option[Rep[Int]]]
    }

/*
 class optionNestedConditional extends ((Int)=>(OptionInt)) {
 def apply(x0:Int): OptionInt = {
 val x1 = x0 == 3
 // TODO: use vars instead of tuples to return multiple values
 val (x6,x7) = if (x1) {
 (x0,true)
 } else {
 val x3 = null.asInstanceOf[Int]
 (x3,false)
 }
 val x8 = new OptionInt(x6,x7)
 x8
 }
 }
 */
  }

  /**
   * same as above, but more elaborate
   */
  def optionNestedConditionalBis(in: Rep[Array[Int]]): Rep[Option[Int]] = {

    val idx = unit(0)
    val first =
      if (idx >= in.length) None.asInstanceOf[Option[Rep[Int]]]
      else if (in(idx) == unit(3)) Some(in(idx))
      else None.asInstanceOf[Option[Rep[Int]]]

    if (first.isDefined) first
    else {
      if (idx >= in.length) None.asInstanceOf[Option[Rep[Int]]]
      else if (in(idx) == unit(5)) Some(in(idx))
      else None.asInstanceOf[Option[Rep[Int]]]
    }

  }
}

class OptionOpsSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testOption = {
    withOutFile(prefix + "option") {
      new OptionProg
          with OptionOpsExpOpt
          with PrimitiveOpsExpOpt
          with NumericOpsExpOpt
          with OrderingOpsExp
          with ArrayOpsExpOpt
          with StructFatExpOptCommon
          with IfThenElseFatExp
          with MyScalaCompile { self =>

        val codegen = new ScalaGenOptionOps
          with ScalaGenPrimitiveOps
          with ScalaGenNumericOps
          with ScalaGenOrderingOps
          with ScalaGenArrayOps
          //with ScalaGenStruct
          with ScalaGenFatStruct
          with ScalaGenIfThenElseFat { val IR: self.type = self }
          //with ScalaGenIfThenElse { val IR: self.type = self }

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

        codegen.emitSource(optionConditional _, "optionConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOptionConditional = compile(optionConditional)
        scala.Console.println(testcOptionConditional(3))
        codegen.reset

        codegen.emitSource(optionConditionalBis _, "optionConditionalBis", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOptionConditionalBis = compile(optionConditionalBis)
        scala.Console.println(testcOptionConditionalBis(3))
        codegen.reset

        codegen.emitSource(optionNestedConditional _, "optionNestedConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOptionNestedConditional = compile(optionNestedConditional)
        scala.Console.println(testcOptionNestedConditional(3))
        codegen.reset

        codegen.emitSource(optionNestedConditionalBis _, "optionNestedConditionalBis", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcOptionNestedConditionalBis = compile(optionNestedConditionalBis)
        scala.Console.println(testcOptionNestedConditionalBis(scala.Array(1)))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "option")
  }
}
