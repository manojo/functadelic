package stagedparsec

import lms._
import scala.lms.common._
import scala.lms.internal.GenericCodegen

/**
 * A trait that specializes readers
 * to use a struct-like representation
 *
 * @see http://manojo.github.io/2015/09/02/staged-parser-combinators/ for more
 * information.
 */
trait StructReaderOps extends ReaderOps {
  abstract class ReaderType[+T]
  type Reader[+T] = ReaderType[T]

  implicit class ReaderCls(rdr: Rep[Input]) {
    def first: Rep[Elem] = rdr_first(rdr)
    def rest: Rep[Input] = rdr_rest(rdr)
    def atEnd: Rep[Boolean] = rdr_atEnd(rdr)
  }

  def rdr_first(rdr: Rep[Input]): Rep[Elem]
  def rdr_rest(rdr: Rep[Input]): Rep[Input]
  def rdr_atEnd(rdr: Rep[Input]): Rep[Boolean]

}

trait StringReaderOps
    extends StructReaderOps
    with ArrayOps
    with OrderingOps
    with NumericOps
    with PrimitiveOps
    with While
    with LiftVariables
    with BooleanOps {

  class StringReader extends Reader[Char]

  type Elem = Char
  type Input = StringReader

  /**
   * Pimp my Library pattern, adding operations specific
   * to StringReader: these operate on inputs which are Array[Char]
   */
  implicit class StringReaderCls(rdr: Rep[StringReader]) extends ReaderCls(rdr) {

    def input: Rep[Array[Elem]] = rdr_input(rdr)
    def offset: Rep[Int] = rdr_offset(rdr)

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
  def rdr_offset(rdr: Rep[Input]): Rep[Int]

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

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def reader_typ: Typ[StringReader] = {
    implicit val ManifestTyp(mT) = typ[Char]
    manifestTyp
  }

  /**
   * creating a string reader as a struct
   */
  def StringReader(input: Rep[Array[Char]], offset: Rep[Int] = unit(0)) = {

    struct(classTag[StringReader],
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
  import IR._

  override def remap[A](m: Typ[A]) = {
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
