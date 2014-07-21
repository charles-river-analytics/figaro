package com.cra.figaro.algorithm.factored

/*
 * DoubleFactor.scala
 * Factor with type Double
 * 
 * Created By: 	Kathryn Rodgers (rodgersk@uci.edu)
 * Creation Date:	7 May 2014
 * 
*/


/**
 * Class with functions specialized for Factor[Double]
 * 
 */
class DoubleFactor(fact:Factor[Double]) {
val semi = SumProductSemiring
	/**
	 * Double Multiplication
	 */
	def *(that:Factor[Double]) = fact.combination(that, (x,y) => x*y)
	/**
	 * Double Addition
	 *
	 * use symbol ++ to avoid string conversions
	 */
	def ++(that:Factor[Double]) = fact.combination(that, (x,y) => x+y)

	/**
	 *  Double Subtraction
	 *  This - That
	 */
	def -(that:Factor[Double]) = fact.combination(that, (x,y) => x-y)

	/**
	 *  Double Division
	 * This / That
	 * 0/0 = 0
	 */
	def /(that:Factor[Double]) = zeroDivide(that)

  /**
   * Power operator
   */
  def ^(power:Double):Factor[Double] = {
    val res = new Factor[Double](fact.variables)
    fact.mapTo((d:Double)=>math.pow(d,power),res)
    res
  }
	/**
	 * Mostly Normal Division
	 * Defines division by zero as NaN
	 * 			division of/by NaN as NaN
	 *    		division of/by +- inf as NaN
	 *      These are Scala defaults
	 *       
	 *       
	 */
	def divide(that:Factor[Double]):Factor[Double] = {
	   def helper(x:Double, y:Double):Double = {
	  if(y == 0  || java.lang.Double.isInfinite(y) )
	    Double.NaN
	   else
		x/y}
	  val res = fact.combination(that, helper)
	  res
	}
	
	/**
	 * Special Division
	 * Defines	division by zero as zero
	 * 			division of zero by zero as zero
	 *    		division of/by NaN by zero as zero
	 *      	division of/by infinity as zero
	 */
	def zeroDivide(that:Factor[Double]):Factor[Double] = {
	  //Special divide operations
	  def divideHelper(x:Double,y:Double):Double = {
	  if (y == 0  || java.lang.Double.isNaN(y) || java.lang.Double.isNaN(x) || java.lang.Double.isInfinite(x) ||  java.lang.Double.isInfinite(y)) 
	    0.0   
	    else  
	      x/y
	  }
	  val res = fact.combination(that, divideHelper)
	  res
	}
	
	/**
	 * Computes the log of the factor's values
	 * 
	 * Returns a factor with the log valuesx
	 */
	def log():Factor[Double] = {
	  val res = new Factor[Double](fact.variables)
	  fact.mapTo((entry) => scala.math.log(entry), res)
	  res
	  
	}
	/**
	 * Computes e^(entry) of the factor's entry values
	 * 
	 * Returns a factor with the exponentiated values
	 */
	def exp() = {
		val res = new Factor[Double](fact.variables)
		fact.mapTo((entry) => scala.math.exp(entry), res)
		res
	  }
	
	/**
   * Returns the summation of the factor over a variable according to the normal addition function.
   * The result is associated with all the variables in the
   * input except for the summed over variable and the value for a set of assignments is the
   * sum of the values of the corresponding assignments in the input.
   */
	def sumOver(variable:Variable[_]) = {
		 fact.sumOver(variable,semi)
		}
	
	/**
   * Returns the maximum value of the factor
   *   
   */
	def max():Double= {
	  fact.max((x,y) => x > y)
	}
	
	 
  /**
   * Returns the maximizing value of the variable(s)
   * @return 
   * List((variable -> variable_value))
   */
	def argMax():List[Pair[Variable[_],Any]] = {
	  fact.argMax((x,y) => x > y)
	  
	}
	
	
}

