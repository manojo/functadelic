package stagedparsec

import lms._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter
import scala.language.implicitConversions

/**
 * an implementation of `Reader` as staged struct
 * based on a previous implementation in
 * https://github.com/manojo/experiments/
 */

/**
 * creating types, for later reuse
 */
abstract class Reader[+T]
class StringReader extends Reader[Char]

trait ReaderOps extends Base {

  type Elem
  type Input <: Reader[Elem]

  //need manifests for later
  implicit val mInput: Manifest[Input]

  implicit class ReaderCls(rdr: Rep[Input]) {
    def first: Rep[Elem] = rdr_first(rdr)
    def rest: Rep[Input] = rdr_rest(rdr)
    def atEnd: Rep[Boolean] = rdr_atEnd(rdr)
    def offset: Rep[Int] = rdr_offset(rdr)
  }

  def rdr_first(rdr: Rep[Input]): Rep[Elem]
  def rdr_rest(rdr: Rep[Input]): Rep[Input]
  def rdr_atEnd(rdr: Rep[Input]): Rep[Boolean]
  def rdr_offset(rdr: Rep[Input]): Rep[Int]
}

trait StringReaderOps
    extends ReaderOps
    with ArrayOps
    with OrderingOps
    with NumericOps
    with PrimitiveOps
    with While
    with LiftVariables
    with BooleanOps {

  type Elem = Char
  type Input = StringReader

  implicit class StringReaderCls(rdr: Rep[StringReader]) extends ReaderCls(rdr) {
    def input: Rep[Array[Elem]] = rdr_input(rdr)

    def foreach(f: Rep[Char] => Rep[Unit]) = {
      var tmp = rdr
      while (!readVar(tmp).atEnd) {
        f(readVar(tmp).first)
        tmp = readVar(tmp).rest
      }
    }
  }

  def rdr_first(rdr: Rep[Input]): Rep[Elem] = rdr.input(rdr.offset)
  def rdr_atEnd(rdr: Rep[Input]): Rep[Boolean] = rdr.offset >= rdr.input.length
  def rdr_rest(rdr: Rep[Input]): Rep[Input] = StringReader(rdr.input, rdr.offset + unit(1))

  def rdr_input(rdr: Rep[StringReader]): Rep[Array[Char]]

  def StringReader(input: Rep[Array[Char]], offset: Rep[Int] = unit(0)): Rep[StringReader]
}

trait StringReaderOpsExp
    extends StringReaderOps
    with ArrayOpsExp
    with OrderingOpsExp
    with NumericOpsExp
    with PrimitiveOpsExp
    with StructExp
    with WhileExp
    with BooleanOpsExp {

  //A bit of a hack: manifest[StringReader] causes a nullPointerException
  //possibly due to a bug in Scala
  implicit val mInput: Manifest[Input] =
    scala.reflect.ManifestFactory.classType[StringReader](classOf[StringReader])

  /**
   * creating a string reader as a struct
   */
  def StringReader(input: Rep[Array[Char]], offset: Rep[Int] = unit(0)) = {

    struct(classTag[StringReader](mInput),
      "input" -> input,
      "offset" -> offset
    )
  }

  def rdr_input(rdr: Rep[StringReader]): Rep[Array[Char]] =
    field[Array[Char]](rdr, "input")

  def rdr_offset(rdr: Rep[StringReader]): Rep[Int] =
    field[Int](rdr, "offset")
}

trait ReaderGenBase extends GenericCodegen with BaseGenStruct {
  val IR: StringReaderOpsExp

  override def remap[A](m: Manifest[A]) = {
    m.erasure.getSimpleName match {
      case "StringReader" => IR.structName(m)
      case _ => super.remap(m)
    }
  }
}

trait StringReaderOpsExpOpt
    extends StringReaderOpsExp
    with ArrayOpsExpOpt
    with NumericOpsExpOpt
    with PrimitiveOpsExpOpt
    with StructExpOptCommon
    with BooleanOpsExpOpt

trait ScalaGenStringReaderOps
    extends ScalaGenBase
    with ReaderGenBase
    with ScalaGenArrayOps
    with ScalaGenOrderingOps
    with ScalaGenNumericOps
    with ScalaGenPrimitiveOps
    with ScalaGenStruct
    with ScalaGenWhile
    with ScalaGenBooleanOps
    with ScalaGenVariables {
  val IR: StringReaderOpsExp
}
