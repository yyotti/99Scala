package problems.logic

object S99Logic {
  import java.io.OutputStream

  // [テンプレート]
  //
  // /**
  //  * P31 (**) Determine whether a given integer number is prime.
  //  *
  //  * scala> 7.isPrime
  //  * res0: Boolean = true
  //  */
  // def isPrime: Boolean = ???

  /**
   * P46 (**) Truth tables for logical expressions.
   * Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which return true or false
   * according to the result of their respective operations;
   * e.g. and(A, B) is true if and only if both A and B are true.
   *
   * scala> and(true, true)
   * res0: Boolean = true
   *
   * scala> xor(true. true)
   * res1: Boolean = false
   *
   * A logical expression in two variables can then be written as an function of two variables,
   * e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))
   *
   * Now, write a function called table2 which prints the truth table of a given logical expression in two variables.
   *
   * scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
   * A     B     result
   * true  true  true
   * true  false true
   * false true  false
   * false false false
   */
  def and(a: Boolean, b: Boolean): Boolean = ???
  def or(a: Boolean, b: Boolean): Boolean = ???
  def nand(a: Boolean, b: Boolean): Boolean = ???
  def nor(a: Boolean, b: Boolean): Boolean = ???
  def xor(a: Boolean, b: Boolean): Boolean = ???
  def impl(a: Boolean, b: Boolean): Boolean = ???
  def equ(a: Boolean, b: Boolean): Boolean = ???
  def table2(f: (Boolean, Boolean) => Boolean)(implicit out: OutputStream = Console.out): Unit = ???
}
