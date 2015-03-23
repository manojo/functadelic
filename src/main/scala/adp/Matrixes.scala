package adp

/**
 * Created by cedricbastin on 20/03/15.
 */

trait MatrixSig extends Signature {

  case class Matrix(x:Int, y:Int)
  case class Cost(v:Int, m:Matrix)


  type Alphabet = Matrix // Input matrices as (rows, columns) we want to find best multiplication sequence
  type Answer = Cost //MatrixAlgebra

  def single(m: Alphabet): Answer
  def mult(l: Answer, r: Answer): Answer
}


trait MatrixGrammar extends BaseParsers with MatrixSig {
  //def tabulate(name:String, inner: => Parser[Answer], alwaysValid:Boolean=false) = new Tabulate(inner,name,alwaysValid)
  //class Tabulate(in: => Parser[Answer], val name:String, val alwaysValid:Boolean=false) extends Parser[Answer]
  // Monoid?
  val simple: Parser[Answer] = el map single

  def chain: Parser[Answer] = (
    (el map single)
  | ((chain ~ chain) map { case (a1, a2) => mult(a1, a2) })
  )

}


trait MatrixAlgebra extends MatrixSig {

  def single(m: Alphabet) = Cost(0, m) //0 cost if no multiplication

  def mult(l: Answer, r: Answer) =
    Cost(
      l.v + r.v + l.m.x * r.m.x * r.m.y,
      Matrix(l.m.x, r.m.y)
    )
}

object Matrixes extends App with MatrixGrammar with MatrixAlgebra {

  //type Alphabet = String
  def input = Array(Matrix(3, 4), Matrix(5, 6), Matrix(6, 7))
  //type Answer = String
  val res = chain.aggregate(ls => List(ls.minBy(_.v)))((0, input.length))

  println(res)
}

