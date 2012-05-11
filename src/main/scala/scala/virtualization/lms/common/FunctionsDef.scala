package scala.virtualization.lms.common

import scala.virtualization.lms.util.ClosureCompare
import scala.reflect.SourceContext

trait FunctionsExternalDef extends FunctionsExp with BlockExp with ClosureCompare {
  case class DefineFun[A,B](res: Block[B])(val arg: Sym[A]) extends Def[A=>B]

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case f@DefineFun(y) => f.arg::effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case DefineFun(y) => freqHot(y)
    case _ => super.symsFreq(e)
  }

  var funTable: List[(Function[_,_], Any, Sym[_])] = List()
  override def doLambda[A:Manifest,B:Manifest](f: Exp[A]=>Exp[B])(implicit pos: SourceContext): Exp[A=>B] = {
    var can = canonicalize(f)

    funTable.find(_._2 == can) match {
      case Some((g, _, funSym)) =>
        funSym.asInstanceOf[Sym[A=>B]]
      case _ =>
      
        var funSym = fresh[A=>B]
        var argSym = fresh[A]//Sym(-1)
      
        val g = (x: Exp[A]) => Apply(funSym, x): Exp[B]
        funTable = (g,can,funSym)::funTable
        
        val y = Block(f(argSym)) // should use reifyEffects!
        
        createDefinition(funSym, DefineFun[A,B](y)(argSym))
        funSym
    }
  }
}

trait ScalaGenFunctionsExternal extends ScalaGenFunctions {
  val IR: FunctionsExternalDef
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@DefineFun(y) =>
      emitValDef(sym, "{" + quote(e.arg) + ": (" + e.arg.tp + ") => "/*}*/)
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}
