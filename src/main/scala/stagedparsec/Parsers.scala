package stagedparsec

import scala.virtualization.lms.common._
import lms._
import lms.util._

/**
 * An implementation of staged parser combinators
 * based on a previous implementation in
 * https://github.com/manojo/experiments/
 */

trait Parsers
    extends ParseResultOps
    with OptionOps
    with ReaderOps
    with MyTupleOps
    with IfThenElse {

  abstract class Parser[+T: Manifest]
      extends (Rep[Input] => Rep[ParseResult[T]]) {


    /**
     * The concat operation
     */
    def ~[U: Manifest](that: Parser[U]) = Parser[(T, U)] { input =>
      val x = this(input)
      if (x.isEmpty) Failure[(T, U)](input)
      else {
        val y = that(x.next)
        if (y.isEmpty) Failure[(T, U)](input)
        else Success(make_tuple2(x.get, y.get), y.next)
      }
    }

  }

  /**
   * companion object for apply function
   */
  object Parser {
    def apply[T: Manifest](f: Rep[Input] => Rep[ParseResult[T]]) = new Parser[T] {
      def apply(in: Rep[Input]) = f(in)
    }

    /**
     * run a parser, and return an `Option`
     */
    def phrase[T: Manifest](p: => Parser[T], in: Rep[Input]): Rep[Option[T]] = {
      val presult = p(in)
      val res = if (presult.isEmpty) none[T]() else Some(presult.get)
      res
    }
  }
}

trait ParsersExp
    extends Parsers
    with ParseResultOpsExp
    with OptionOpsExp
    with MyTupleOpsExp
    with IfThenElseExpOpt
    with BooleanOpsExpOpt
    with EqualExpOpt


trait ScalaGenParsers
    extends ScalaGenParseResultOps
    with ScalaGenOptionOps
    with ScalaGenMyTupleOps
    with ScalaGenIfThenElse
    with ScalaGenBooleanOps
    with ScalaGenEqual {
  val IR: ParsersExp
}
