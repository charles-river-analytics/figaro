package com.cra.figaro.experimental.structured.strategy

import com.cra.figaro.experimental.structured._
import com.cra.figaro.experimental.structured.solver.Solver
import com.cra.figaro.language.Element

class StructuredSolver(problem: Problem, solver: Solver,
                       recursingStrategy: Strategy, rangeSizer: RangeSizer,
                       bounds: Bounds, parameterized: Boolean) {
  def execute() {
    backwardChain(problem.components, Set[ProblemComponent[_]]())
    problem.solve(solver)
  }

  /*
   * backwardChain takes a list of items to process.
   * When the first item is taken off the list, it checks whether it has already been done.
   * When an item is taken off the list, if it has not been done, all the items it depends on
   * are added to the list to do. This guarantees that when an item is finally processed,
   * all the items it depends on have already been processed. Also, we do not process
   * any items more than once.
   */
  private def backwardChain(toDo: List[ProblemComponent[_]], done: Set[ProblemComponent[_]]): Set[ProblemComponent[_]] = {
    toDo match {
      case first :: rest =>
        // globals should have been processed before this problem
        if (done.contains(first) || !problem.contains(first.problem)) backwardChain(rest, done)
        else {
          val argComponents = first.element.args.map(checkArg(_))
          val done1 = if (argComponents.nonEmpty) backwardChain(argComponents, done) else done
          first match {
            case chainComp: ChainComponent[_,_] =>
              chainComp.expand()
              chainComp.subproblems.values.foreach(recurse(_, done))
              process(chainComp)
              backwardChain(rest, done1 + chainComp)
            case maComp: MakeArrayComponent[_] =>
              maComp.expand()
              val items = maComp.makeArray.items.take(maComp.maxExpanded).toList
              val itemComponents = items.map(checkArg(_))
              val done2 = backwardChain(itemComponents ::: toDo, done1)
              process(maComp)
              backwardChain(rest, done2 + maComp)
            case _ =>
              process(first)
              backwardChain(rest, done1 + first)
          }
        }
      case _ => done
    }
  }

  private def process(comp: ProblemComponent[_]) {
    comp.generateRange(rangeSizer(comp))
    comp.makeNonConstraintFactors(parameterized)
    comp.makeConstraintFactors(bounds)
  }

  private def recurse(nestedProblem: NestedProblem[_], done: Set[ProblemComponent[_]]) {
    // This check is important and is what enables us to perform dynamic programming
    if (!nestedProblem.solved) recursingStrategy(nestedProblem)
  }

  private def checkArg[T](element: Element[T]): ProblemComponent[T] = {
    if (problem.collection.contains(element)) problem.collection(element)
    else problem.add(element)
  }
}
