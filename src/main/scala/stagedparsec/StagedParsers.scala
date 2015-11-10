package stagedparsec

import scala.lms.common._
import lms._
import lms.util._
import barbedwire.FoldLefts

/**
 * An implementation of staged parser combinators
 * based on a previous implementation in
 * https://github.com/manojo/experiments/
 *
 * And now it changes! We use CPS encodings of a ParseResult now.
 * @see http://manojo.github.io/2015/09/02/staged-parser-combinators/ for more
 * information.
 */

trait StagedParsers
    extends ParseResultCPS
    with OptionOps
    with ReaderOps
    with TupleOps
    with IfThenElse
    with ParseResultOps {

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
    def ~[U: Typ](that: => Parser[U]): Parser[(T, U)] =
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
    def | (that: => Parser[T]) = Parser[T] { input =>
      this(input) orElse that(input)
    }

    /**
     * creates an explicit Rep[ParseResult] from running the parser
     * This function is useful for creating join points at function boundaries,
     * and typically handy for recurive parsers.
     */
    def toParseResult: Rep[Input] => Rep[ParseResult[T]] = (in: Rep[Input]) => {
      var isEmpty = unit(true); var tmpRes = unit(zeroVal[T]); var nxt = in
      this(in).apply(
        (t, next) => { isEmpty = unit(false); tmpRes = t; nxt = next },
        (next) => { nxt = next }
      )

      if (isEmpty) Failure[T](nxt) else Success(tmpRes, nxt)
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

  /**
   * a combinator for creating a recursive parser
   * Why do we need an explicit one? Because our parsers are code generators.
   * And the code that needs to be generated changes based on where
   * it is supposed to be generated. In order to not use an explicit recursion
   * combinator we'd have to inspect the parser structure using reflection
   * otherwise. The combinator is a reasonable tradeoff for the effort.
   */
  def rec[T: Typ](p: Parser[T]): Parser[T]

  /**
   * The basic repetition combinator.
   * In a staged setting, instead of a list, we produce a CPSList,
   * also known as a FoldLeft
   */
  //def rep[T: Typ](p: Parser[T]): Parser[FoldLeft[T]] = Parser { in => new FoldLeft[T] {
  //  def apply[S: Typ](z: Rep[S], comb: Comb[T, S]): Rep[S] = z
  //}}
}

trait StagedParsersExp
    extends StagedParsers
    with ParseResultCPSExp
    with OptionOpsExp
    with TupleOpsExp
    with IfThenElseExp
    with BooleanOpsExp
    with EqualExp
    with ParseResultOpsExp
    with FunctionsExp {

  /** A dictionary to see which parsers already have corresponding symbols */
  val store = new scala.collection.mutable.HashMap[Parser[_], Sym[_]]

  /**
   * for recursion of parsers, we can alas not implement it using
   * just DSL code, we need to get hands dirty with the internals
   * of our tree/graph representation.
   * We also can't simply re-use recursive functions as implemented in LMS
   *
   * Inspired from
   * https://github.com/manojo/experiments/blob/simple/src/main/scala/lms/parsing/TopDownParsers.scala
   *
   * @see http://manojo.github.io/2015/09/04/staged-parser-combinators-recursion/
   * for more info
   */
  def rec[T: Typ](p: Parser[T]) = Parser[T] { in =>

    import Parser._

    val myFun: Rep[Input => ParseResult[T]] = store.get(p) match {

      case Some(f) =>
        scala.Console.println("we have a function call")
        val realf = f.asInstanceOf[Exp[Input => ParseResult[T]]]
        realf

      case None =>
        scala.Console.println("first time we see this guy, creating a new symbol")

        val funSym = fresh[Input => ParseResult[T]]

        store += (p -> funSym)
        val f = p.toParseResult
        val g = createDefinition(funSym, doLambdaDef(f))
        store -= p

        funSym
    }

    val res: Rep[ParseResult[T]] = myFun(in)
    conditional(res.isEmpty,
      ParseResultCPS.Failure(res.next),
      ParseResultCPS.Success(res.get, res.next)
    )

  }
}

trait StagedParsersExpOpt
    extends StagedParsersExp
    with OptionOpsExpOpt
    with IfThenElseExpOpt
    with BooleanOpsExpOpt
    with EqualExpOpt
    with ParseResultOpsExpOpt

trait ScalaGenStagedParsers
    extends ScalaGenParseResultCPS
    with ScalaGenOptionOps
    with ScalaGenTupleOps
    with ScalaGenIfThenElse
    with ScalaGenBooleanOps
    with ScalaGenEqual
    with ScalaGenParseResultOps
    with ScalaGenFunctions {
  val IR: StagedParsersExp
}
