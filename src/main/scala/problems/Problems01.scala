package problems

import java.util.NoSuchElementException
import scala.annotation.tailrec

object WorkingWithLists {
  // [テンプレート]
  //
  // /**
  //  * P01 (*) Find the last element of a list.
  //  *
  //  * Example:
  //  *   scala> last(List(1, 1, 2, 3, 5, 8))
  //  *   res0: Int = 8
  //  */
  // def last[A](list: List[A]): A = ???

  /**
   * P01 (*) Find the last element of a list.
   *
   * Example:
   *   scala> last(List(1, 1, 2, 3, 5, 8))
   *   res0: Int = 8
   */
  def last[A](list: List[A]): A = list.last

  /**
   * P02 (*) Find the last but one element of a list.
   *
   * Example:
   *   scala> penultimate(List(1, 1, 2, 3, 5, 8))
   *   res0: Int = 5
   */
  def penultimate[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException
    case _ => list.init.last
  }

  /**
   * P03 (*) Find the Kth element of a list.
   * By convention, the first element in the list is element 0.
   *
   * Example:
   *   scala> nth(2, List(1, 1, 2, 3, 5, 8))
   *   res0: Int = 2
   */
  def nth[A](n: Int, list: List[A]): A = list(n)

  /**
   * P04 (*) Find the number of elements of a list.
   *
   * Example:
   *   scala> length(List(1, 1, 2, 3, 5, 8))
   *   res0: Int = 6
   */
  def length[A](list: List[A]): Int = list.size

  /**
   * P05 (*) Reverse a list.
   *
   * Example:
   *   scala> reverse(List(1, 1, 2, 3, 5, 8))
   *   res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  def reverse[A](list: List[A]): List[A] = list.reverse

  /**
   * P06 (*) Find out whether a list is a palindrome.
   *
   * Example:
   *   scala> isPalindrome(List(1, 2, 3, 2, 1))
   *   res0: Boolean = true
   */
  def isPalindrome[A](list: List[A]): Boolean = reverse(list) == list

  /**
   * P07 (**) Flatten a nested list structure.
   *
   * Example:
   *   scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
   *   res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   */
  def flatten(list: List[_]): List[Any] = list.flatMap {
    case xs: List[_] => flatten(xs)
    case x => List(x)
  }


  /**
   * P08 (**) Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
   *
   * Example:
   *   scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *   res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   */
  def compress[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: xs => x :: compress(xs.dropWhile { _ == x })
  }

  /**
   * P09 (**) Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   *
   * Example:
   *   scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *   res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   */
  def pack[A](list: List[A]): List[List[A]] = list match {
    case Nil => Nil
    case x :: _ => list.takeWhile { _ == x } :: pack(list.dropWhile { _ == x })
  }

  /**
   * P10 (*) Run-length encoding of a list.
   * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
   * Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
   *
   * Example:
   *   scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *   res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encode[A](list: List[A]): List[(Int, A)] = pack(list).map { xs => (xs.size, xs.head) }

  /**
   * P11 (*) Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
   *
   * Example:
   *   scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *   res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   */
  def encodeModified[A](list: List[A]): List[Any] = encode(list).map {
    case (c, x) if c == 1 => x
    case x => x
  }

  /**
   * P12 (**) Decode a run-length encoded list.
   * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
   *
   * Example:
   *   scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
   *   res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   */
  def decode[A](list: List[(Int, A)]): List[A] = list.flatMap { case (c, x) => List.fill(c)(x) }

  /**
   * P13 (**) Run-length encoding of a list (direct solution).
   * Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
   *
   * Example:
   *   scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *   res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encodeDirect[A](list: List[A]): List[(Int, A)] = list match {
    case Nil => Nil
    case x :: _ =>
      val (packed, xs) = list.span { _ == x }
      (packed.size, x) :: encodeDirect(xs)
  }

  /**
   * P14 (*) Duplicate the elements of a list.
   *
   * Example:
   *   scala> duplicate(List('a, 'b, 'c, 'c, 'd))
   *   res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   */
  def duplicate[A](list: List[A]): List[A] = list.flatMap { x => List(x, x) }

  /**
   * P15 (**) Duplicate the elements of a list a given number of times.
   *
   * Example:
   *   scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
   *   res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
   */
  def duplicateN[A](n: Int, list: List[A]): List[A] =
    if (n < 0) throw new IllegalArgumentException
    else list.flatMap { x => List.fill(n)(x) }

  /**
   * P16 (**) Drop every Nth element from a list.
   *
   * Example:
   *   scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   *   res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
   */
  def drop[A](n: Int, list: List[A]): List[A] =
    if (n < 0) throw new IllegalArgumentException
    else if (n == 0) list
    else list.grouped(n).flatMap {
      case x if (x.size == n) => x.init
      case x => x
    }.toList

  /**
   * P17 (*) Split a list into two parts.
   * The length of the first part is given. Use a Tuple for your result.
   *
   * Example:
   *   scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   *   res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   */
  def split[A](n: Int, list: List[A]): (List[A], List[A]) = ???
}
