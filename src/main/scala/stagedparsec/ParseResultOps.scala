package stagedparsec

import lms._

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * A struct representation for Parse Results
 * Inspired from TupleOps on delite-develop branch
 */
trait ParseResultOps
    extends Base
    with IfThenElse
    with BooleanOps { self: ReaderOps =>

  /**
   * creating a type for use in generating structs
   */
  abstract class ParseResult[+T: Manifest]

  def Success[T: Manifest](res: Rep[T], next: Rep[Input]): Rep[ParseResult[T]]
  def Failure[T: Manifest](next: Rep[Input]): Rep[ParseResult[T]]

  implicit class ParseResultCls[A: Manifest](pr: Rep[ParseResult[A]]) {
    def isEmpty: Rep[Boolean] = parseresult_isEmpty(pr)
    def get: Rep[A] = parseresult_get(pr)
    def orElse(that: Rep[ParseResult[A]]) = parseresult_orElse(pr, that)
    def next: Rep[Input] = parseresult_next(pr)

    def map[B: Manifest](f: Rep[A] => Rep[B]) = parseresult_map(pr, f)
  }

  def parseresult_isEmpty[A: Manifest](pr: Rep[ParseResult[A]])
                                      (implicit pos: SourceContext): Rep[Boolean]
  def parseresult_get[A: Manifest](pr: Rep[ParseResult[A]])
                                  (implicit pos: SourceContext): Rep[A]
  def parseresult_next[A: Manifest](pr: Rep[ParseResult[A]])
                                   (implicit pos: SourceContext): Rep[Input]

  def parseresult_orElse[A: Manifest](pr: Rep[ParseResult[A]], that: Rep[ParseResult[A]])
                                     (implicit pos: SourceContext): Rep[ParseResult[A]] =
    if (pr.isEmpty) that else pr

  def parseresult_map[A: Manifest, B: Manifest](pr: Rep[ParseResult[A]], f: Rep[A] => Rep[B])
                                               (implicit pos: SourceContext): Rep[ParseResult[B]] =
    if (pr.isEmpty) Failure[B](pr.next) else Success(f(pr.get), pr.next)

}

trait ParseResultOpsExp
    extends ParseResultOps
    with IfThenElseExp
    with BooleanOpsExp
    with StructExp { self: ReaderOps =>

  def parseresult_isEmpty[A: Manifest](pr: Rep[ParseResult[A]])
                                      (implicit pos: SourceContext): Rep[Boolean] =
    field[Boolean](pr, "empty")

  def parseresult_get[A: Manifest](pr: Rep[ParseResult[A]])
                                  (implicit pos: SourceContext): Rep[A] =
    field[A](pr, "res")

  def parseresult_next[A: Manifest](pr: Rep[ParseResult[A]])
                                   (implicit pos: SourceContext): Rep[Input] =
    field[Input](pr, "next")

  def Success[T: Manifest](res: Rep[T], next: Rep[Input]): Exp[ParseResult[T]] =
    struct(
      classTag[ParseResult[T]],
      "res" -> res,
      "empty" -> unit(false),
      "next" -> next
    )

  def Failure[T: Manifest](next: Rep[Input]): Exp[ParseResult[T]] =
    struct(
      classTag[ParseResult[T]],
      "res" -> unit(ZeroVal[T]),
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

  override def remap[A](m: Manifest[A]) = m.erasure.getSimpleName match {
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
