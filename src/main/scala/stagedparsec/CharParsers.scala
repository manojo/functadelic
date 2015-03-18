package stagedparsec

import scala.virtualization.lms.common._
import lms._
import lms.util._

/**
 * This trait contains basic parsers for characters
 * as input elements
 */
trait CharParsers
    extends StagedParsers
    with CharOps
    with StringReaderOps {

  /**
   * The elementary parser. Accepts a character if it passes
   * the given predicate
   */
  def acceptIf(p: Rep[Elem] => Rep[Boolean]) = Parser[Char] { in =>
    if (in.atEnd) Failure[Char](in)
    else if (p(in.first)) Success(in.first, in.rest)
    else Failure[Char](in)
  }

  def accept(e: Rep[Elem]): Parser[Char] = acceptIf(_ == e)

  /**
   * elementary recognisers. Parse a character, simply return
   * the next index to be processed from
   */
  def acceptIdx(e: Rep[Elem]): Parser[Int] = acceptIfIdx(_ == e)

  def acceptIfIdx(p: Rep[Elem] => Rep[Boolean]) = Parser[Int] { in =>
    if (in.atEnd) Failure[Int](in)
    else if (p(in.first)) Success(in.offset, in.rest)
    else Failure[Int](in)
  }

  def isLetter(c: Rep[Char]): Rep[Boolean] =
    (c >= unit('a') && c <= unit('z')) ||
    (c >= unit('A') && c <= unit('Z'))

  def letter: Parser[Char] = acceptIf(isLetter)
  def letterIdx = acceptIfIdx(isLetter)

  def isDigit(c: Rep[Char]): Rep[Boolean] =
    c >= unit('0') && c <= unit('9')

  def digit: Parser[Char] = acceptIf(isDigit)
  def digit2Int: Parser[Int] = digit map (c => (c - unit('0')).toInt)
  def digitIdx = acceptIfIdx(isDigit)

}

trait CharParsersExp
    extends CharParsers
    with StagedParsersExp
    with CharOpsExp
    with StringReaderOpsExp

trait ScalaGenCharParsers
    extends ScalaGenStagedParsers
    with ScalaGenCharOps
    with ScalaGenStringReaderOps {
  val IR: CharParsersExp
}
