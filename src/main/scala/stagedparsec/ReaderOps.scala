package stagedparsec

import scala.lms.common._

/**
 * an implementation of `Reader` as staged struct
 * based on a previous implementation in
 * https://github.com/manojo/experiments/
 *
 * @see http://manojo.github.io/2015/09/02/staged-parser-combinators/ for more
 * information.
 */


trait ReaderOps extends Base {

  /**
   * An abstract type corresponding
   * to the actual representation of a Reader
   * This is done so that we can either use
   * structs, or other (ex. CPS) representations
   * of a Reader in subtraits
   */
  type Reader[+T]

  type Elem
  type Input <: Reader[Elem]

  /**
   * Typs
   */
  implicit def reader_typ: Typ[Input]

  /**
   * It would be ideal to give a specification of the operations
   * that a Reader can perform here. But it's actually not that
   * easy, because in the CPS-encoded case we seek to avoid the notion
   * of a Rep[Reader] while in the struct case we have them. Hence the spec
   * given in the form of comments:
   *
   * A Reader implementation should confer the following operations on a
   * Reader/Rep[Reader]:
   *    * def first: Rep[Elem]
   *    * def rest: Rep[Input]
   *    * def atEnd: Rep[Boolean]
   *    * def offset: Rep[Int]
   */
}
