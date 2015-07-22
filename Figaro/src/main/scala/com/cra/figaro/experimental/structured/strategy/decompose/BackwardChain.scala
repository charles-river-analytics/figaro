/*
 * BackwardChain.scala
 * A trait for a backward chaining algorithm for decomposition strategies
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   July 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.experimental.structured.strategy.decompose

import com.cra.figaro.experimental.structured._
import com.cra.figaro.language._

trait BackwardChain extends DecompositionStrategy {

  /*
   * backwardChain takes a list of items to process.
   * When the first item is taken off the list, it checks whether it has already been done.
   * When an item is taken off the list, if it has not been done, all the items it depends on
   * are added to the list to do. This guarantees that when an item is finally processed,
   * all the items it depends on have already been processed. Also, we do not process
   * any items more than once.
   */
  protected[figaro] def backwardChain(toDo: List[ProblemComponent[_]], done: Set[ProblemComponent[_]]): Set[ProblemComponent[_]] = {
    toDo match {
      case first :: rest =>
        // globals should have been processed before this problem
        if (done.contains(first) || !problem.contains(first.problem)) backwardChain(rest, done)
        else {
          val argComponents = first.element.args.map(checkArg(_))
          val done1 = if (argComponents.nonEmpty) backwardChain(argComponents, done) else done
          first match {
            case chainComp: ChainComponent[_, _] =>
              processChain(first, rest, done1, chainComp)
            case maComp: MakeArrayComponent[_] =>
              processMakeArray(first, rest, done1, maComp)
            case _ =>
              process(first)
              backwardChain(rest, done1 + first)
          }
        }
      case _ => done
    }
  }

  protected def processChain(first: ProblemComponent[_], rest: List[ProblemComponent[_]], done: Set[ProblemComponent[_]], chainComp: ChainComponent[_, _]): Set[ProblemComponent[_]]
  
  protected def processMakeArray(first: ProblemComponent[_], rest: List[ProblemComponent[_]], done: Set[ProblemComponent[_]], maComp: MakeArrayComponent[_]): Set[ProblemComponent[_]]
  
}