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
  type Result = (Double, Char)

  abstract class Category
  sealed case class Terminal(results: Result*) extends Category
  sealed case class Nonterminal(productions: Production*) extends Category

  type Task = (Category, Int)

//  val generate: Task => Element[S] = (task: Task) => {
//    val (cat, len) = task
//    cat match {
//      case term: Terminal =>
//        if (len == 1) Constant(S(false, Select(term.results:_*), Constant(S.empty))) else Constant(S.empty)
//      case nonterm: Nonterminal =>
//        if (len <= 1) Constant(S.empty) else {
//          val firstLen = FromRange(1, len)
//          val secondLen = Apply(firstLen, (p: Int) => len - p)
//          val probsWithIndices = for { (prod, i) <- nonterm.productions.zipWithIndex } yield (probability(prod), i)
//          val production: Element[ProductionBody] = Select(nonterm.productions:_*)
//          val firstCat: Element[Category] = Apply(production, (b: ProductionBody) => b._1())
//          val secondCat: Element[Category] = Apply(production, (b: ProductionBody) => b._2())
//          val firstTask: Element[Task] = ^^(firstCat, firstLen)
//          val secondTask: Element[Task] = ^^(secondCat, secondLen)
//          val seq1: Element[S] = Chain(firstTask, generate)
//          val seq2: Element[S] = Chain(secondTask, generate)
//          Apply(seq1, seq2, (s1: S, s2: S) => S.append(s1, s2))
//        }
//    }
//  }
//
//  def check(evidence: String): S => Element[Boolean] = (s: S) => {
//    if (s.isEmpty && evidence.isEmpty) Constant(true)
//    else if (s.isEmpty || evidence.isEmpty) Constant(false)
//    else {
//      Apply(s.head, (c: Char) => c == evidence(0)) &&
//        Chain(s.tail, (s2: S) => check(evidence.drop(1))(s2))
//    }
//  }


//  val generate: Task => Element[Option[String]] = (task: Task) => {
//    def concat(os1: Option[String], os2: Option[String]) = {
//      (os1, os2) match {
//        case (Some(s1), Some(s2)) => Some(s1 + s2)
//        case _ => None
//      }
//    }
//    val (cat, len) = task
//    cat match {
//      case term: Terminal =>
//        if (len == 1) Select(term.results.map(pair => (pair._1, Some(pair._2))):_*) else Constant(None)
//      case nonterm: Nonterminal =>
//        if (len <= 1) Constant(None) else {
//          val firstLen = FromRange(1, len)
//          val secondLen = Apply(firstLen, (p: Int) => len - p)
//          val probsWithIndices = for { (prod, i) <- nonterm.productions.zipWithIndex } yield (probability(prod), i)
//          val production: Element[ProductionBody] = Select(nonterm.productions:_*)
//           val firstCat: Element[Category] = Apply(production, (b: ProductionBody) => b._1())
//          val secondCat: Element[Category] = Apply(production, (b: ProductionBody) => b._2())
//          val firstTask: Element[Task] = ^^(firstCat, firstLen)
//          val secondTask: Element[Task] = ^^(secondCat, secondLen)
//          val seq1: Element[Option[String]] = Chain(firstTask, generate)
//          val seq2: Element[Option[String]] = Chain(secondTask, generate)
//          Apply(seq1, seq2, concat _)
//        }
//    }
//  }

  val checkers = scala.collection.mutable.Map[String, C]()
  
  def Checker(seq: String): C = {
    checkers.get(seq) match {
      case Some(c) => c
      case None =>
        val result = new C(seq)
        checkers += seq -> result
        result
    }
  }
    
    
  
  class C(seq: String) {  
    val check: Category => Element[Boolean] = (cat: Category) => {
//println("In check: seq = " + seq + ", cat = " + cat)      
      cat match {
        case term: Terminal =>
          if (seq.size == 1) {
            Select(term.results:_*).map(_ == seq(0))
          } else Constant(false)
        case nonterm: Nonterminal =>
          if (seq.size < 2) Constant(false)
          else {
            val probsWithIndices = for { (prod, i) <- nonterm.productions.zipWithIndex } yield (probability(prod), i)
            val index = Select(probsWithIndices:_*)
            val production = index.map(nonterm.productions(_))
            val possibilities =
              for { len <- 1 until seq.size } yield {
                val sub1 = seq.slice(0, len)
                val sub2 = seq.slice(len, seq.size)
                val cat1 = production.map(body1(_)())
                val cat2 = production.map(body2(_)())
                val check1 = Chain(cat1, Checker(sub1).check)
                val check2 = Chain(cat2, Checker(sub2).check)
                (check1 && check2)
              }
            Reduce((b1: Boolean, b2: Boolean) => b1 || b2)(possibilities:_*)
          }
      }
    }
  }
}
