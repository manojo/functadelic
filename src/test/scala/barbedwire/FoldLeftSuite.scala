package barbedwire

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects
import lms._
import lms.util._

//only importing this to access the type
import java.util.HashMap

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

/**
 * Basic test suite for foldleft
 */

trait FoldLeftProg extends FoldLefts with Equal with HashMapOps {

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

  /**
   * map append map over a range
   */
  def mapappendmapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange[List[Int]](a, b)

    val mapped = xs map (_ * unit(2))
    ((mapped :+ (b + unit(1))) map (_ * unit(3))).apply(List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * partition map
   */
  def partitionmapRange(a: Rep[Int], b: Rep[Int]): Rep[(List[Int], List[Int])] = {
    val xs = FoldLeft.fromRange[List[Int]](a, b)
    val (evens, odds) = xs.partition(_ % unit(2) == unit(0))
    val (mappedEvens, mappedOdds) = (evens map (_ * unit(2)) , odds map (_ * unit(3)))
    val evenList = (evens map (_ * unit(2))).apply(List[Int](), (ls, x) => ls ++ List(x))
    val oddList = (odds map (_ * unit(3))).apply(List[Int](), (ls, x) => ls ++ List(x))

    make_tuple2(evenList, oddList)
  }

  /**
   * a "multifoldleft". fold over two sequences.
   * Just to see what happens
   */
  def multifoldleftRange(a: Rep[Int], b: Rep[Int]): Rep[(List[Int], List[Int])] = {
    val xs = FoldLeft.fromRange[(List[Int], List[Int])](a, b)
    xs.apply(
      make_tuple2((List[Int](), List[Int]())),
      (acc, elem) =>
        if (elem % unit(2) == unit(0)) make_tuple2((acc._1 ++ List(elem), acc._2))
        else make_tuple2((acc._1, acc._2 ++ List(elem)))
    )
  }

  /**
   *  partition bis. Just a really basic fold with nothing more
   */
  def partitionbis(a: Rep[Int], b: Rep[Int]): Rep[List[Either[Int, Int]]] = {
    val xs = FoldLeft.fromRange[List[Either[Int, Int]]](a, b)
    val partitioned = (xs.partitionBis(_ % unit(2) == unit(0)))
    partitioned.apply(List[Either[Int, Int]](), (ls, x) => ls ++ List(x))
  }

  /**
   * partition bis map.
   */
  def partitionbismap(a: Rep[Int], b: Rep[Int]): Rep[List[Either[Int, Int]]] = {
    val xs = FoldLeft.fromRange[List[Either[Int, Int]]](a, b)
    val partitioned = (xs.partitionBis(_ % unit(2) == unit(0)))
    val mapped = partitioned map { x => x.fold(_ * unit(2), _ * unit(3)) }
    mapped.apply(List[Either[Int, Int]](), (ls, x) => ls ++ List(x))
  }

  /**
   * partition bis map, folded into a pair of lists
   */
  def partitionbismap2listpair(a: Rep[Int], b: Rep[Int]): Rep[(List[Int], List[Int])] = {
    val xs = FoldLeft.fromRange[(List[Int], List[Int])](a, b)
    val partitioned = (xs.partitionBis(_ % unit(2) == unit(0)))
    val mapped = partitioned map { x => x.fold(_ * unit(2), _ * unit(3)) }
    mapped.apply(
      (List[Int](), List[Int]()),
      (ls, x) =>
        if (x.isLeft) (ls._1 ++ List(x.getLeft), ls._2)
        else (ls._1, ls._2 ++ List(x.getRight))
    )
  }

  /**
   * groupWith followed by sum
   */
  def groupwithsum(a: Rep[Int], b: Rep[Int]): Rep[HashMap[Int, Int]] = {
    val xs = FoldLeft.fromRange[HashMap[Int, Int]](a, b)
    val grouped = xs.groupWith(x => x % unit(3))

    grouped.apply(
      HashMap[Int, Int](),
      (dict, x) =>
        if (dict.contains(x._1)) { dict.update(x._1, dict(x._1) + x._2); dict }
        else { dict.update(x._1, x._2); dict }
    )
  }
}

/**
 * A trait that mixes all the relevant Exp traits that are required for this example
 * The corresponding codegen trait as well
 */
trait FoldLeftExp extends ListOpsExpOpt with IfThenElseExpOpt with BooleanOpsExpOpt with VariablesExpOpt
   with OrderingOpsExp with NumericOpsExpOpt with PrimitiveOpsExpOpt with WhileExp with EqualExpOpt
   with EitherOpsExp with HashMapOpsExp

trait FoldLeftGen extends ScalaGenListOps with ScalaGenIfThenElse with ScalaGenBooleanOps with ScalaGenVariables
   with ScalaGenOrderingOps with ScalaGenNumericOps with ScalaGenPrimitiveOps with ScalaGenWhile
   with ScalaGenEqual with ScalaGenEitherOps with ScalaGenHashMapOps {
  val IR: FoldLeftExp
}

class FoldLeftSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testFoldLeft = {
    withOutFile(prefix + "foldleft") {
      new FoldLeftProg with FoldLeftExp with MyTupleOpsExp with MyScalaCompile { self =>

        val codegen = new FoldLeftGen with ScalaGenMyTupleOps { val IR: self.type = self }

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

        codegen.emitSource2(mapappendmapRange _, "mapappendmapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapappendmapRange = compile2(mapappendmapRange)
        scala.Console.println(testcMapappendmapRange(1, 5))
        codegen.reset
      }
    }

    assertFileEqualsCheck(prefix + "foldleft")
  }

  def testPartition = {
    withOutFile(prefix + "partition") {
      new FoldLeftProg with FoldLeftExp with MyTupleOpsExp with MyScalaCompile { self =>

        val codegen = new FoldLeftGen with ScalaGenMyTupleOps { val IR: self.type = self }

        codegen.emitSource2(partitionmapRange _, "partitionmapRange", new java.io.PrintWriter(System.out))
        codegen.emitDataStructures(new java.io.PrintWriter(System.out))
        codegen.reset

        val testcPartitionmapRange = compile2(partitionmapRange)
        scala.Console.println(testcPartitionmapRange(1, 10))
        codegen.reset

        codegen.emitSource2(multifoldleftRange _, "multifoldleftRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMultifoldleftRange = compile2(multifoldleftRange)
        scala.Console.println(testcMultifoldleftRange(1, 10))
        codegen.reset

        codegen.emitSource2(partitionbis _, "partitionbis", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcPartitionbis = compile2(partitionbis)
        scala.Console.println(testcPartitionbis(1, 10))
        codegen.reset

        codegen.emitSource2(partitionbismap _, "partitionbismap", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcPartitionbismap = compile2(partitionbismap)
        scala.Console.println(testcPartitionbismap(1, 10))
        codegen.reset

        codegen.emitSource2(partitionbismap2listpair _, "partitionbismap2listpair", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcPartitionbismap2listpair = compile2(partitionbismap2listpair)
        scala.Console.println(testcPartitionbismap2listpair(1, 10))
        codegen.reset

        codegen.emitSource2(groupwithsum _, "groupwithsum", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcGroupwithsum = compile2(groupwithsum)
        scala.Console.println(testcGroupwithsum(1, 10))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "partition")
  }
}
