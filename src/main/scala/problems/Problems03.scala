package problems.logic

object S99Logic {
  import java.io.OutputStream
  import scala.language.implicitConversions

  implicit def boolean2S99Boolean(b: Boolean) = new S99Boolean(b)

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
  def and(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _ => false
  }
  def or(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (false, false) => false
    case _ => true
  }
  def not(a: Boolean): Boolean = a match {
    case true => false
    case _ => true
  }
  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))
  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))
  def xor(a: Boolean, b: Boolean): Boolean = or(and(a, not(b)), and(not(a), b))
  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)
  def equ(a: Boolean, b: Boolean): Boolean = not(xor(a, b))
  def table2(f: (Boolean, Boolean) => Boolean)(implicit out: OutputStream = Console.out): Unit =
    Console.withOut(out) {
      println("A     B     result")
      Seq(true, false).map { a =>
        Seq(true, false).map { b =>
          println(f"$a%-5s $b%-5s ${f(a, b)}")
        }
      }
    }

  /**
   * P49 (**) Gray code.
   * An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
   * n = 1: C(1) = ("0", "1").
   * n = 2: C(2) = ("00", "01", "11", "10").
   * n = 3: C(3) = ("000", "001", "011", "010", "110", "111", "101", "100").
   *
   * Find out the construction rules and write a function to generate Gray codes.
   *
   * scala> gray(3)
   * res0 List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)
   *
   * See if you can use memoization to make the function more efficient.
   */
  def gray(n: Int): List[String] =
    if (n < 0) throw new IllegalArgumentException
    else if (n == 0) List("")
    else {
      val codes = gray(n - 1)
      codes.map { c => "0" + c } ::: codes.reverse.map { c => "1" + c }
    }

  private val grayCodes = collection.mutable.Map(0 -> List(""))
  def grayMemorized(n: Int): List[String] =
    if (n < 0) throw new IllegalArgumentException
    else grayCodes.getOrElseUpdate(n, { val codes = grayMemorized(n - 1); codes.map { c => "0" + c } ::: codes.reverse.map { c => "1" + c } })

  /**
   * P50 (***) Huffman code.
   * First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes!
   * We suppose a set of symbols with their frequencies, given as a list of (S, F) Tuples.
   * E.g. (("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)).
   * Our objective is to construct a list of (S, C) Tuples, where C is the Huffman code word for the symbol S.
   *
   * scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
   * res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))
   */
  sealed abstract class HuffmanTree[A](val freq: Int) {
    def toCodeList(prefix: String): List[(A, String)]
  }
  case class HuffmanNode[A](
    override val freq: Int,
    left: Option[HuffmanTree[A]],
    right: Option[HuffmanTree[A]]
  ) extends HuffmanTree[A](freq) {
    def toCodeList(prefix: String): List[(A, String)] =
      left.map { _.toCodeList(prefix + "0") }.getOrElse(Nil) ::: right.map { _.toCodeList(prefix + "1") }.getOrElse(Nil)
  }
  case class HuffmanLeaf[A](value: A, override val freq: Int) extends HuffmanTree[A](freq) {
    def toCodeList(prefix: String): List[(A, String)] = List((value, prefix))
  }
  def huffman[A: Ordering](freqs: List[(A, Int)]): List[(A, String)] = {
    @annotation.tailrec
    def createTree(nodes: List[HuffmanTree[A]]): HuffmanTree[A] = nodes.sortBy { _.freq } match {
      case Nil => throw new java.util.NoSuchElementException
      case x :: Nil => HuffmanNode(x.freq, Some(x), None)
      case x :: y :: Nil => HuffmanNode(x.freq + y.freq, Some(x), Some(y))
      case x :: y :: zs => createTree(HuffmanNode(x.freq + y.freq, Some(x), Some(y)) :: zs)
    }

    freqs match {
      case Nil => Nil
      case _ => createTree(freqs.map { case (v, f) => HuffmanLeaf(v, f) }).toCodeList("").sortBy { _._1 }
    }
  }
}

/**
 * P47 (*) Truth tables for logical expressions (2).
 * Continue problem P46 by redefining and, or, etc as operators.
 * (i.e. make them methods of a new class with an implicit conversion from Boolean.)
 * not will have to be left as a object method.
 *
 * scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
 * A     B     result
 * true  true  true
 * true  false true
 * false true  false
 * false false false
 */
class S99Boolean(val b: Boolean) {
  def and(a: Boolean): Boolean = S99Logic.and(b, a)
  def or(a: Boolean): Boolean = S99Logic.or(b, a)
  def nand(a: Boolean): Boolean = S99Logic.nand(b, a)
  def nor(a: Boolean): Boolean = S99Logic.nor(b, a)
  def xor(a: Boolean): Boolean = S99Logic.xor(b, a)
  def impl(a: Boolean): Boolean = S99Logic.impl(b, a)
  def equ(a: Boolean): Boolean = S99Logic.equ(b, a)
}
