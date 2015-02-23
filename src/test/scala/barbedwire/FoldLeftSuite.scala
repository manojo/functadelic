package barbedwire

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects
import lms._

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

/**
 * Basic test suite for foldleft
 */

trait FoldLeftProg extends FoldLefts with Equal {

  /**
   * simple foldLeft back into a list
   */
  def foldLeftId(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))
    val fld = FoldLeft.fromList[Int, List[Int]](xs)

    fld.apply(List[Int](), (ls, x) => ls ++ List(x))

  }

  /**
   * map, test if it inlines
   */
  def map(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))
    val mapped = FoldLeft.fromList[Int, List[Int]](xs) map (_ * unit(2))

    mapped.apply(List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * map map, test if it inlines
   */
  def mapmap(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))

    val mapped = FoldLeft.fromList[Int, List[Int]](xs) map (_ * unit(2))
    (mapped map (_ + unit(1))).apply(List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * map map over a range
   */
  def mapmapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange[List[Int]](a, b)

    val mapped = xs map (_ * unit(2))
    (mapped map (_ + unit(1))).apply(List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * filter over a range
   */
  def filterRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange[List[Int]](a, b)

    val filtered = xs filter (_ % unit(2) == unit(1))
    filtered.apply(List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * filter map over a range
   */
  def filtermapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange[List[Int]](a, b)

    val filtered = xs filter (_ % unit(2) == unit(1))
    filtered.map(_ * unit(3)).apply(List[Int](), (ls, x) => ls ++ List(x))
  }


  /**
   * flatMap over a range
   */
  def flatMapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange[List[Int]](a, b)

    val flatMapped = xs flatMap (i => FoldLeft.fromRange[List[Int]](unit(1), i))
    flatMapped.apply(List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * flatMap map over a range
   */
  def flatMapmapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange[List[Int]](a, b)

    val flatMapped = xs flatMap (i => FoldLeft.fromRange[List[Int]](unit(1), i))
    flatMapped.map(_ * unit(2)).apply(List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * flatMap filter map over a range
   */
  def flatMapfiltermapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange[List[Int]](a, b)

    val flatMapped = xs flatMap (i => FoldLeft.fromRange[List[Int]](unit(1), i))
    val filtered = flatMapped filter (_ % unit(2) == unit(1))
    filtered.map(_ * unit(3)).apply(List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * map concat map over a range
   */
  def mapconcatmapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange[List[Int]](a, b)
    val ys = FoldLeft.fromRange[List[Int]](a, b)

    val mapped = xs map (_ * unit(2))
    ((mapped ++ ys) map (_ * unit(3))).apply(List[Int](), (ls, x) => ls ++ List(x))
  }


}

/**
 * A trait that mixes all the relevant Exp traits that are required for this example
 * The corresponding codegen trait as well
 */
trait FoldLeftExp extends ListOpsExpOpt with IfThenElseExpOpt with BooleanOpsExpOpt with VariablesExpOpt
   with OrderingOpsExp with NumericOpsExpOpt with PrimitiveOpsExpOpt with WhileExp with EqualExpOpt

trait FoldLeftGen extends ScalaGenListOps with ScalaGenIfThenElse with ScalaGenBooleanOps with ScalaGenVariables
   with ScalaGenOrderingOps with ScalaGenNumericOps with ScalaGenPrimitiveOps with ScalaGenWhile with ScalaGenEqual {
  val IR: FoldLeftExp
}

class FoldLeftSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testFoldLeft = {
    withOutFile(prefix + "foldleft") {
      new FoldLeftProg with FoldLeftExp with MyScalaCompile { self =>

        val codegen = new FoldLeftGen { val IR: self.type = self }

        codegen.emitSource(foldLeftId _, "foldLeftId", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcfoldLeftId = compile(foldLeftId)
        scala.Console.println(testcfoldLeftId(1))
        codegen.reset

        codegen.emitSource(map _, "map", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMap = compile(map)
        scala.Console.println(testcMap(1))
        codegen.reset

        codegen.emitSource(mapmap _, "mapmap", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapmap = compile(mapmap)
        scala.Console.println(testcMapmap(1))
        codegen.reset

        codegen.emitSource2(mapmapRange _, "mapmapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapmapRange = compile2(mapmapRange)
        scala.Console.println(testcMapmapRange(1, 5))
        codegen.reset

        codegen.emitSource2(filterRange _, "filterRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFilterRange = compile2(filterRange)
        scala.Console.println(testcFilterRange(1, 5))
        codegen.reset

        codegen.emitSource2(filtermapRange _, "filtermapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFiltermapRange = compile2(filtermapRange)
        scala.Console.println(testcFiltermapRange(1, 5))
        codegen.reset

        codegen.emitSource2(flatMapRange _, "flatMapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapRange = compile2(flatMapRange)
        scala.Console.println(testcFlatMapRange(1, 5))
        codegen.reset

        codegen.emitSource2(flatMapmapRange _, "flatMapmapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapmapRange = compile2(flatMapmapRange)
        scala.Console.println(testcFlatMapmapRange(1, 5))
        codegen.reset

        codegen.emitSource2(flatMapfiltermapRange _, "flatMapfiltermapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapfiltermapRange = compile2(flatMapfiltermapRange)
        scala.Console.println(testcFlatMapfiltermapRange(1, 5))
        codegen.reset

        codegen.emitSource2(mapconcatmapRange _, "mapconcatmapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapconcatmapRange = compile2(mapconcatmapRange)
        scala.Console.println(testcMapconcatmapRange(1, 5))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "foldleft")
  }
}
