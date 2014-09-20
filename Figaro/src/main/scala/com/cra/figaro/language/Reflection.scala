/*
 * Reflection.scala
 * Supports creating Figaro elements from names in compilers
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

import scala.reflect.runtime.{ universe => ru }

/**
 * Figaro's reflection allows you to create a Figaro element by providing the name of the element class as a string and its arguments as elements.
 * This can be useful, e.g., for compilers.
 * In order to be able to create an instance of a particular element class, the class must implement the Creatable trait.
 */

trait Creatable {
  /** The type over which the element is defined. */ 
  type ResultType
  
  /** Create an element of this type with the given arguments. */
  def create(args: List[Element[_]]): Element[ResultType]
}

object Create {
  /** Create an element with the given class name and inputs. The class name must name a creatable element class. */
  def apply[T](className: String, inputs: Element[_]*): Element[T] = {
    val module = java.lang.Class.forName(className)
    val rootMirror = ru.runtimeMirror(module.getClassLoader)
    val moduleSymbol = rootMirror.moduleSymbol(module)
    val moduleMirror = rootMirror.reflectModule(moduleSymbol)
    val obj = moduleMirror.instance
    if (!obj.isInstanceOf[Creatable]) throw new IllegalArgumentException("Attempt to use non-reflectable external distribution")
    val objType = ru.typeOf[Creatable]
    val objMirror = rootMirror.reflect(obj)
    val methodSymbol = objType.decl(ru.TermName("create")).asMethod
    val methodMirror = objMirror.reflectMethod(methodSymbol)
    try {
      methodMirror(inputs.toList).asInstanceOf[Element[T]]
    } catch {
      case _: java.lang.reflect.InvocationTargetException => throw new RuntimeException("Wrong argument types for external distribution")
      case _: ClassCastException => throw new RuntimeException("Wrong result type for external distribution")
    }
  }
}
