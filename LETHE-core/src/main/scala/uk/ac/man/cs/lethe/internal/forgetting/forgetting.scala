
package uk.ac.man.cs.lethe.internal.forgetting

abstract class Forgetter[FORMULA, SYMBOL] { 
  def forget(formula: FORMULA, symbols: Set[SYMBOL]): FORMULA
  def steps: Int // Steps needed for last operation
}

trait ForgetterWithBackgroundKB[FORMULA, SYMBOL] {
  def forget(formula: FORMULA, symbols: Set[SYMBOL], backgroundKB: FORMULA): FORMULA
}



trait Interpolator[FORMULA, SYMBOL] { 
  def uniformInterpolant(formula: FORMULA, symbols: Set[SYMBOL]): FORMULA
}

trait InterpolatorWithBackgroundKB[FORMULA, SYMBOL] extends Interpolator[FORMULA, SYMBOL] { 
  def uniformInterpolant(formula: FORMULA, symbols: Set[SYMBOL], backgroundKB: FORMULA): FORMULA
}
