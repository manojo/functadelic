package stagedparsec

import lms._

import scala.lms.common._
import scala.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * A struct representation for Parse Results
 * Inspired from TupleOps on delite-develop branch
 */
trait ParseResultOps
    extends Base
    with IfThenElse
    with BooleanOps
    with ZeroVal { self: ReaderOps =>

  /**
   * creating a type for use in generating structs
   */
  abstract class ParseResult[T: Typ]

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def parseresult_typ[A: Typ]: Typ[ParseResult[A]]

  def Success[T: Typ](res: Rep[T], next: Rep[Input]): Rep[ParseResult[T]]
  def Failure[T: Typ](next: Rep[Input]): Rep[ParseResult[T]]

  implicit class ParseResultCls[A: Typ](pr: Rep[ParseResult[A]]) {
    def isEmpty: Rep[Boolean] = parseresult_isEmpty(pr)
    def get: Rep[A] = parseresult_get(pr)
    def orElse(that: Rep[ParseResult[A]]) = parseresult_orElse(pr, that)
    def next: Rep[Input] = parseresult_next(pr)

    def map[B: Typ](f: Rep[A] => Rep[B]) = parseresult_map(pr, f)
  }

  def parseresult_isEmpty[A: Typ](pr: Rep[ParseResult[A]])
                                 (implicit pos: SourceContext): Rep[Boolean]
  def parseresult_get[A: Typ](pr: Rep[ParseResult[A]])
                             (implicit pos: SourceContext): Rep[A]
  def parseresult_next[A: Typ](pr: Rep[ParseResult[A]])
                              (implicit pos: SourceContext): Rep[Input]

  def parseresult_orElse[A: Typ](pr: Rep[ParseResult[A]], that: Rep[ParseResult[A]])
                                (implicit pos: SourceContext): Rep[ParseResult[A]] =
    if (pr.isEmpty) that else pr

  def parseresult_map[A: Typ, B: Typ](pr: Rep[ParseResult[A]], f: Rep[A] => Rep[B])
                                     (implicit pos: SourceContext): Rep[ParseResult[B]] =
    if (pr.isEmpty) Failure[B](pr.next) else Success(f(pr.get), pr.next)

}

trait ParseResultOpsExp
    extends ParseResultOps
    with IfThenElseExp
    with BooleanOpsExp
    with StructExp
    with ZeroValExp { self: ReaderOps =>

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def parseresult_typ[A: Typ]: Typ[ParseResult[A]] = {
    implicit val ManifestTyp(mA) = typ[A]
    manifestTyp
  }

  def parseresult_isEmpty[A: Typ](pr: Rep[ParseResult[A]])
                                 (implicit pos: SourceContext): Rep[Boolean] =
    field[Boolean](pr, "empty")

  def parseresult_get[A: Typ](pr: Rep[ParseResult[A]])
                             (implicit pos: SourceContext): Rep[A] =
    field[A](pr, "res")

  def parseresult_next[A: Typ](pr: Rep[ParseResult[A]])
                              (implicit pos: SourceContext): Rep[Input] =
    field[Input](pr, "next")

  def Success[T: Typ](res: Rep[T], next: Rep[Input]): Exp[ParseResult[T]] =
    struct(
      classTag[ParseResult[T]],
      "res" -> res,
      "empty" -> unit(false),
      "next" -> next
    )

  def Failure[T: Typ](next: Rep[Input]): Exp[ParseResult[T]] =
    struct(
      classTag[ParseResult[T]],
      "res" -> unit(zeroVal[T]),
      "empty" -> unit(true),
      "next" -> next
    )
}

trait ParseResultOpsExpOpt
    extends ParseResultOpsExp
    with IfThenElseExpOpt
    with BooleanOpsExpOpt
    with StructExpOptCommon
    with EqualExpOpt { self: ReaderOps => }

trait ParseResultGenBase extends GenericCodegen with BaseGenStruct {
  val IR: ParseResultOpsExp
  import IR._

  override def remap[A](m: Typ[A]) = m.erasure.getSimpleName match {
    case "ParseResult" => IR.structName(m)
    case _ => super.remap(m)
  }
}

trait ScalaGenParseResultOps
    extends ScalaGenBase
    with ParseResultGenBase
    with ScalaGenStruct
    with ScalaGenIfThenElse
    with ScalaGenBooleanOps {
  val IR: ParseResultOpsExp
}

/*
trait CGenParseResultOps
    extends CGenBase
    with ParseResultGenBase
    with CGenStructOps {
  val IR: ParseResultOpsExp
}
*/
