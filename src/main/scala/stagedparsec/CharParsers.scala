package stagedparsec

import scala.lms.common._
import lms._
import lms.util._

/**
 * This trait contains basic parsers for characters
 * as input elements
 *
 * @see http://manojo.github.io/2015/09/02/staged-parser-combinators/ for more
 * information.
 */
trait CharParsers
    extends StagedParsers
    with CharOps
    with StringReaderOps {

  /**
   * Some primitive parsers
   */

  /**
   * The elementary parser. Accepts a character if it passes
   * the given predicate
   * NOTE: we should technically be able to write
   * !in.atEnd && p(in.first)
   * Because we are using a staged struct representation though,
   * literally writing that would generate code from computing
   * in.first *before* evaluating the atEnd part, and this can
   * cause issues (IndexOutOfBounds etc.)
   */
  def acceptIf(p: Rep[Elem] => Rep[Boolean]) = Parser[Elem] { in =>
    conditional(
      in.atEnd,
      ParseResultCPS.Failure[Elem](in),
      conditional(
        p(in.first),
        ParseResultCPS.Success(in.first, in.rest),
        ParseResultCPS.Failure[Elem](in)
      )
    )
  }

  def accept(e: Rep[Elem]): Parser[Elem] = acceptIf(_ == e)

  /**
   * elementary recognisers. Parse a character, simply return
   * the next index to be processed from
   */
  def acceptIdx(e: Rep[Elem]): Parser[Int] = acceptIfIdx(_ == e)

  def acceptIfIdx(p: Rep[Elem] => Rep[Boolean]) = Parser[Int] { in =>
    conditional(
      in.atEnd,
      ParseResultCPS.Failure[Int](in),
      conditional(
        p(in.first),
        ParseResultCPS.Success(in.offset, in.rest),
        ParseResultCPS.Failure[Int](in)
      )
    )
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

trait CharParsersExpOpt
    extends CharParsersExp
    with StagedParsersExpOpt
    with StringReaderOpsExpOpt

trait ScalaGenCharParsers
    extends ScalaGenStagedParsers
    with ScalaGenCharOps
    with ScalaGenStringReaderOps {
  val IR: CharParsersExp
}
