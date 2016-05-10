package lms.util

import lms._

import scala.virtualization.lms.common._
import scala.reflect.SourceContext

trait StringStructOps extends lms.StructOps with While with IfThenElse
    with NumericOps with ArrayOps with Equal with StringOps with OrderingOps
    with BooleanOps with CharOps with PrimitiveOps with StaticData with MiscOps
    with LiftVariables {

  type StringStruct = lms.Record {
    val input: Array[Char]
    val start: Int
    val length: Int
  }

  def String(in: Rep[Array[Char]], st: Rep[Int], len: Rep[Int] = unit(0)) = new lms.Record {
    val input = in
    val start = st
    val length = len
  }

  def toLower(c: Rep[Char]): Rep[Char] =
    (c.toInt | unit(0x20)).toChar

  //unsound operation in general, we need to have the same input !!
  /*  def infix_+(l: Rep[StringStruct], r: Rep[StringStruct])(implicit pos: SourceContext)
    = stringStruct_plus(l,r)
  def stringStruct_plus(l: Rep[StringStruct], r: Rep[StringStruct])(implicit pos: SourceContext) : Rep[StringStruct]
    = String(in = l.input, st = l.start, len = (r.length:Rep[Int]))
*/
  /*FIXME: for now always assume r is lower cased*/
  def __equal(l: Rep[StringStruct], r: Rep[Array[Char]])(implicit pos: SourceContext): Rep[Boolean] = {
    if (l.length == r.length) {
      val in: Rep[Array[Char]] = l.input
      val st = l.start
      var i = unit(0); var tmp = unit(true)
      while (i < l.length && tmp) {
        if (toLower(in(i + st)) != r(i)) {
          tmp = unit(false)
        }
        i = i + unit(1)
      }
      tmp
    } else {
      unit(false)
    }
  }

  def __equal(l: Rep[StringStruct], r: String)(implicit pos: SourceContext): Rep[Boolean] = {
    //TODO: lms.Array.apply not working
    //val tmp: Rep[Array[Char]] = array_obj_fromseq{
    //  staticData(r.toArray)
    //}
    l == staticData(r.toArray)
  }

  def infix_mkString(st: Rep[StringStruct])(implicit pos: SourceContext): Rep[String] = {
    var s = unit(""); var i = unit(0)
    while (i < st.length) {
      s = string_plus(s, st.input(st.start + readVar(i)))
      i = i + unit(1)
    }
    s
  }

  def infix_toStr(st: Rep[StringStruct])(implicit pos: SourceContext): Rep[String]

}

trait StringStructOpsExp extends StringStructOps with StructOpsExpOptCommon with WhileExp with IfThenElseExpOpt
    with BooleanOpsExp with NumericOpsExp with ArrayOpsExp with EqualExpOpt with StringOpsExp with OrderingOpsExp
    with CharOpsExp with PrimitiveOpsExp with MiscOpsExp with VariablesExp with StaticDataExp {
  case class StringStructToString(s: Rep[StringStruct]) extends Def[String]
  def infix_toStr(s: Rep[StringStruct])(implicit pos: SourceContext) = StringStructToString(s)
}

trait ScalaGenStringStructOps extends ScalaGenBase with ScalaGenStructOps with ScalaGenWhile with ScalaGenIfThenElse
    with ScalaGenNumericOps with ScalaGenArrayOps with ScalaGenEqual with ScalaGenStringOps with ScalaGenOrderingOps
    with ScalaGenBooleanOps with ScalaGenCharOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenVariables with ScalaGenStaticData {
  val IR: StringStructOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringStructToString(s) => emitValDef(sym, src"$s.input.slice($s.start,$s.start+$s.length).mkString")
    case _ => super.emitNode(sym, rhs)
  }
}
