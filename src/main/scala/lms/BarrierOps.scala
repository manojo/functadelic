/**
 * Barrier synchronization using simple effects
 */
package lms

import lms._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Effects

import java.io.PrintWriter
import scala.reflect.SourceContext

trait BarrierOps extends Base {
  def barrierSync(cmt: String)(implicit pos: SourceContext): Rep[Unit]
}

trait BarrierOpsExp extends BarrierOps with EffectExp {
  case class BarrierSync(cmt: String) extends Def[Unit]

  def barrierSync(cmt: String)(implicit pos: SourceContext) = {
    printlog("warning: inserting barrier sync.")
    printsrc(raw"in $pos")
    reflectEffect(BarrierSync(cmt)) // Simple effect
  }

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(BarrierSync(cmt), u, es) => reflectMirrored(Reflect(BarrierSync(cmt), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenBarrierOps extends ScalaGenEffect {
  val IR: BarrierOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BarrierSync(cmt) => gen"""// Barrier sync: $cmt"""
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenBarrierOps extends CGenEffect {
  val IR: BarrierOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BarrierSync(cmt) => gen"""/* Barrier sync: $cmt */"""
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenBarrierOps extends CudaGenEffect {
  val IR: BarrierOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BarrierSync(cmt) => gen"""/* Barrier sync: $cmt */"""
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenBarrierOps extends OpenCLGenEffect {
  val IR: BarrierOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BarrierSync(cmt) => gen"""/* Barrier sync: $cmt */"""
    case _ => super.emitNode(sym, rhs)
  }
}
