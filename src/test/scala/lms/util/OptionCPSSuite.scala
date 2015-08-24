package lms.util

import scala.lms.common._
import scala.reflect.SourceContext
import scala.lms.internal.GenericCodegen
import lms._

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

/** stupid feature import*/
import scala.language.postfixOps

trait OptionCPSProg
    extends OptionCPS
    with OptionOps
    with OrderingOps
    with PrimitiveOps
    with NumericOps {

  import OptionCPS._

  /**
   * NOTE: we use `conditional` instead of the classic
   * if then else sugar, because there is either the virtualized
   * version (which required Reps for everything) or classic
   * version (no Reps anywhere)
   */

  def singleConditional(in: Rep[Int]): Rep[Option[Int]] = {
    val c = conditional((in <= unit(3)),
      Some(in),
      None[Int]
    )
    c.toOption
  }

  def nestedConditional(in: Rep[Int]): Rep[Option[Int]] = {
    val c = conditional(
      in <= unit(3),
      conditional(in >= unit(1), Some(in), None[Int]),
      conditional(in >= unit(5), Some(in), None[Int])
    )
    c.toOption
  }

  /**
   * The same conditional expression twice
   * to make sure that no optimization collapses the conditional
   */
  def nestedConditional2(in: Rep[Int]): Rep[Option[Int]] = {
    val c = conditional(
      in == unit(3),
      Some(in),
      conditional(in == unit(3), Some(in), None[Int])
    )
    c.toOption
  }

  /**
   * should generate code where the notion of option
   * has disappeard
   */
  def mapSome(in: Rep[Int]): Rep[Option[Int]] = {
    val s = Some(in)
    s.map(x => x * unit(2)).toOption
  }

  /**
   * should generate code where the notion of option
   * has disappeard. In particular the function passed
   * to map is not generated
   */
  def mapNone(in: Rep[Int]): Rep[Option[Int]] = {
    val s = None[Int]
    s.map(x => x * unit(2)).toOption
  }

  def mapConditional(in: Rep[Int]): Rep[Option[Int]] = {
    val s = conditional(in <= unit(3), Some(in), None[Int])
    s.map(_ * unit(2)).toOption
  }

  def mapConditional2(in: Rep[Int]): Rep[Option[Int]] = {
    val s = conditional(in <= unit(3), Some(in), Some(in * unit(2)))
    s.map(_ * unit(3)).toOption
  }

  def mapNestedConditional(in: Rep[Int]): Rep[Option[Int]] = {
    val s = conditional(
      in <= unit(3),
      conditional(in >= unit(1), Some(in), None[Int]),
      Some(in * unit(2))
    )
    s.map(_ * unit(3)).toOption
  }

  /**
   * should generate code where the notion of option
   * has disappeard
   */
  def flatMapSome(in: Rep[Int]): Rep[Option[Int]] = {
    val s = Some(in)
    s.flatMap(x => Some(x * unit(2))).toOption
  }

  /**
   * should generate code where the notion of option
   * has disappeard
   */
  def flatMapNone(in: Rep[Int]): Rep[Option[Int]] = {
    val s = None[Int]
    s.flatMap(x => Some(x * unit(2))).toOption
  }

  def flatMapConditional(in: Rep[Int]): Rep[Option[Int]] = {
    val s = conditional(in <= unit(3), Some(in), None[Int])
    s flatMap { x =>
      conditional(
        x >= unit(1),
        Some(x * unit(5)),
        Some(x * unit(10))
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

class OptionCPSSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testOptionCPS = {
    withOutFile(prefix + "optioncps") {
      new OptionCPSProg
          with OptionOpsExp
          with OrderingOpsExpOpt
          with PrimitiveOpsExpOpt
          with NumericOpsExpOpt
          /** this trait should be mixed in higher up */ with ArrayOpsExp
          /** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new ScalaGenBase
            with ScalaGenIfThenElse
            with ScalaGenBooleanOps
            with ScalaGenOrderingOps
            with ScalaGenEqual
            with ScalaGenVariables
            with ScalaGenOptionOps
            with ScalaGenPrimitiveOps
            with ScalaGenNumericOps { val IR: self.type = self }

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

        codegen.emitSource(mapSome _, "mapSome", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapSome = compile(mapSome)
        scala.Console.println(testcMapSome(5))
        codegen.reset

        codegen.emitSource(mapNone _, "mapNone", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapNone = compile(mapNone)
        scala.Console.println(testcMapNone(5))
        codegen.reset

        codegen.emitSource(mapConditional _, "mapConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapConditional = compile(mapConditional)
        scala.Console.println(testcMapConditional(5))
        codegen.reset

        codegen.emitSource(mapConditional2 _, "mapConditional2", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapConditional2 = compile(mapConditional2)
        scala.Console.println(testcMapConditional2(5))
        codegen.reset

        codegen.emitSource(mapNestedConditional _, "mapNestedConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapNestedConditional = compile(mapNestedConditional)
        scala.Console.println(testcMapNestedConditional(5))
        codegen.reset

        codegen.emitSource(flatMapSome _, "flatMapSome", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapSome = compile(flatMapSome)
        scala.Console.println(testcFlatMapSome(5))
        codegen.reset

        codegen.emitSource(flatMapNone _, "flatMapNone", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapNone = compile(flatMapNone)
        scala.Console.println(testcFlatMapNone(5))
        codegen.reset

        codegen.emitSource(flatMapConditional _, "flatMapConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapConditional = compile(flatMapConditional)
        scala.Console.println(testcFlatMapConditional(5))
        scala.Console.println(testcFlatMapConditional(3))
        scala.Console.println(testcFlatMapConditional(0))
        codegen.reset


      }
    }

    assertFileEqualsCheck(prefix + "optioncps")
  }
}
