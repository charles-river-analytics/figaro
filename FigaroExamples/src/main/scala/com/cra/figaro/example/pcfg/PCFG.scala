package pcfg

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete.FromRange

object PCFG {
  type ProductionBody = (() => Category, () => Category)
  type Production = (Double, ProductionBody)
  def probability(p: Production) = p._1
  def body1(p: Production) = p._2._1
  def body2(p: Production) = p._2._2
  type Result = (Double, String)

  abstract class Category
  sealed case class Terminal(results: Result*) extends Category
  sealed case class Nonterminal(productions: Production*) extends Category

  type Task = (Category, Int)

  val generate: Task => Element[Option[String]] = (task: Task) => {
    def concat(os1: Option[String], os2: Option[String]) = {
      (os1, os2) match {
        case (Some(s1), Some(s2)) => Some(s1 + s2)
        case _ => None
      }
    }
    val (cat, len) = task
    if (len <= 0) Constant(None)
    else cat match {
      case term: Terminal =>
        if (len == 1) Select(term.results.map(Some(_)):_*) else Constant(None)
      case nonterm: Nonterminal =>
        val firstLen = FromRange(1, len)
        val secondLen = Apply(firstLen, (p: Int) => len - p)
        val probsWithIndices = for { (prod, i) <- nonterm.productions.zipWithIndex } yield (probability(prod), i)
        val production: Element[ProductionBody] = Select(nonterm.productions:_*)
        val firstCat: Element[Category] = Apply(production, (b: ProductionBody) => b._1())
        val secondCat: Element[Category] = Apply(production, (b: ProductionBody) => b._2())
        val firstTask: Element[Task] = ^^(firstCat, firstLen)
        val secondTask: Element[Task] = ^^(secondCat, secondLen)
        val seq1: Element[Option[String]] = Chain(firstTask, generate)
        val seq2: Element[Option[String]] = Chain(secondTask, generate)
        Apply(seq1, seq2, concat _)
    }
  }

//  def check[T](cat: Category[T], seq: Seq[T]): Element[Boolean] = {
//    cat match {
//      case term: Terminal[T] =>
//        if (seq.size == 1) {
//          Select(term.results:_*).map(_ == seq(0))
//        } else Constant(false)
//      case nonterm: Nonterminal[T] =>
//        if (seq.size < 2) Constant(false)
//        else {
//          val probsWithIndices = for { (prod, i) <- nonterm.productions.zipWithIndex } yield (probability(prod), i)
//          val checkProduction: Int => Element[Boolean] = (index: Int) => {
//            val production = nonterm.productions(index)
//              val possibilities =
//                for { len <- 1 until seq.size } yield {
//                  val sub1 = seq.slice(0, len)
//                  val sub2 = seq.slice(len, seq.size)
//                  val check1 = check(body1(production), sub1)
//                  val check2 = check(body2(production), sub2)
//                  (check1 && check2)
//                }
//            Reduce((b1: Boolean, b2: Boolean) => b1 || b2)(possibilities:_*)
//          }
//          Chain(Select(probsWithIndices:_*), checkProduction)
//        }
//    }
//  }
}
