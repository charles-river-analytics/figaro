package pcfg

import com.cra.figaro.language._
import com.cra.figaro.library.compound.If

case class S(isEmpty: Boolean, head: Element[Char], tail: Element[S])

object S {
  def cons(head: Element[Char], tail: Element[S]) = S(false, head, tail)

  val empty = S(true, null, null)

  def append(s1: S, s2: S): S = 
    if(s1.isEmpty) s2 else cons(s1.head, Apply(s1.tail, (t: S) => append(t, s2)))
}

