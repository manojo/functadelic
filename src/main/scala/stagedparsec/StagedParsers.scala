package stagedparsec

import scala.lms.common._
import lms._
import lms.util._

/**
 * An implementation of staged parser combinators
 * based on a previous implementation in
 * https://github.com/manojo/experiments/
 *
 * And now it changes! We use CPS encodings of a ParseResult now
 */

trait StagedParsers
    extends ParseResultCPS
    with OptionOps
    with ReaderOps
    with TupleOps
    with IfThenElse {

  /** We make a parser invariant at the moment*/
  abstract class Parser[T: Typ]
      extends (Rep[Input] => ParseResultCPS[T]) {

    /**
     * The flatMap operation
     */
    private def flatMap[U: Typ](f: Rep[T] => Parser[U]) = Parser[U] { input =>
      this(input) flatMapWithNext { (res, rdr) => f(res)(rdr) }
    }

    def >>[U: Typ](f: Rep[T] => Parser[U]) = flatMap(f)

    /**
     * The concat operation
     */
    def ~[U: Typ](that: Parser[U]): Parser[(T, U)] =
      for (l <- this; r <- that) yield make_tuple2(l, r)

    /**
     * get right hand side result
     */
    def ~>[U: Typ](that: => Parser[U]): Parser[U] =
      this flatMap { l => that }

    /**
     * get left hand side result
     */
    def <~[U: Typ](that: => Parser[U]): Parser[T] =
      for (l <- this; r <- that) yield l


    /**
     * The map operation
     */
    def map[U: Typ](f: Rep[T] => Rep[U]) = Parser[U] { input =>
      this(input) map f
    }

    /**
     * alternation, aka the beast
     * Note: actually, it's not much of a beast nomore!
     */
    def | (that: Parser[T]) = Parser[T] { input =>
      this(input) orElse that(input)
    }
  }

  /**
   * a 'conditional' parser
   * lifts conditional expressions to parser level
   */
  def __ifThenElse[A: Typ](
    cond: Rep[Boolean],
    thenp: => Parser[A],
    elsep: => Parser[A]
  ): Parser[A] = Parser[A] { input => conditional(cond, thenp(input), elsep(input)) }

  /**
   * companion object for apply function
   */
  object Parser {
    def apply[T: Typ](f: Rep[Input] => ParseResultCPS[T]) = new Parser[T] {
      def apply(in: Rep[Input]) = f(in)
    }

    /**
     * run a parser, and return an `Option`
     */
    def phrase[T: Typ](p: => Parser[T], in: Rep[Input]): Rep[Option[T]] =
      p(in).toOption
  }
}


trait StagedParsersExp
    extends StagedParsers
    with ParseResultCPSExp
    with OptionOpsExp
    with TupleOpsExp
    with IfThenElseExp
    with BooleanOpsExp
    with EqualExp

trait StagedParsersExpOpt
    extends StagedParsersExp
    with ParseResultOpsExpOpt
    with OptionOpsExpOpt
    with IfThenElseExpOpt
    with BooleanOpsExpOpt
    with EqualExpOpt

trait ScalaGenStagedParsers
    extends ScalaGenParseResultCPS
    with ScalaGenOptionOps
    with ScalaGenTupleOps
    with ScalaGenIfThenElse
    with ScalaGenBooleanOps
    with ScalaGenEqual {
  val IR: StagedParsersExp
}
