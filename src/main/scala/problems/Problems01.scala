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
  @tailrec
  def last[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException
    case x :: Nil => x
    case _ :: xs => last(xs)
  }

  /**
   * P02 (*) Find the last but one element of a list.
   *
   * Example:
   *   scala> penultimate(List(1, 1, 2, 3, 5, 8))
   *   res0: Int = 5
   */
  @tailrec
  def penultimate[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException
    case x :: Nil => throw new NoSuchElementException
    case x :: _ :: Nil => x
    case _ :: xs => penultimate(xs)
  }

  /**
   * P03 (*) Find the Kth element of a list.
   * By convention, the first element in the list is element 0.
   *
   * Example:
   *   scala> nth(2, List(1, 1, 2, 3, 5, 8))
   *   res0: Int = 2
   */
  @tailrec
  def nth[A](n: Int, list: List[A]): A =
    if (n < 0) throw new IndexOutOfBoundsException
    else (n, list) match {
      case (_, Nil) => throw new IndexOutOfBoundsException
      case (0, x :: _) => x
      case (n, _ :: xs) => nth(n - 1, xs)
    }

  /**
   * P04 (*) Find the number of elements of a list.
   *
   * Example:
   *   scala> length(List(1, 1, 2, 3, 5, 8))
   *   res0: Int = 6
   */
  def length[A](list: List[A]): Int = list match {
    case Nil => 0
    case _ :: xs => 1 + length(xs)
  }

  /**
   * P05 (*) Reverse a list.
   *
   * Example:
   *   scala> reverse(List(1, 1, 2, 3, 5, 8))
   *   res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseR(list: List[A], rev: List[A]): List[A] = list match {
      case Nil => rev
      case x :: xs => reverseR(xs, x :: rev)
    }

    reverseR(list, Nil)
  }

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
  def flatten(list: List[_]): List[Any] = {
    def append(list1: List[_], list2: List[_]): List[Any] = {
      @tailrec
      def appendR(list1: List[_], list2: List[_], list: List[_]): List[Any] = (list1, list2) match {
        case (Nil, Nil) => reverse(list)
        case (Nil, _) => appendR(list2, Nil, list)
        case (x :: xs, _) => appendR(xs, list2, x :: list)
      }

      appendR(list1, list2, Nil)
    }

    list match {
      case Nil => Nil
      case (x: List[_]) :: xs => append(flatten(x), flatten(xs))
      case x :: xs => x :: flatten(xs)
    }
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
    case x1 :: x2 :: xs if (x1 == x2) => compress(x2 :: xs)
    case x :: xs => x :: compress(xs)
  }

  /**
   * P09 (**) Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   *
   * Example:
   *   scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *   res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   */
  def pack[A](list: List[A]): List[List[A]] = {
    @tailrec
    def takeSame(list: List[A], sublist: List[A]): (List[A], List[A]) = (list, sublist) match {
      case (Nil, _) => (Nil , sublist)
      case (x :: xs, Nil) => takeSame(xs, x :: Nil)
      case (x :: xs, y :: ys) if (x == y) => takeSame(xs, x :: sublist)
      case _ => (list, sublist)
    }

    takeSame(list, Nil) match {
      case (_, Nil) => Nil
      case (list, sublist) => sublist :: pack(list)
    }
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
  def encode[A](list: List[A]): List[(Int, A)] = {
    @tailrec
    def countSame(list: List[A], count: Option[(Int, A)]): (List[A], Option[(Int, A)]) = (list, count) match {
      case (Nil, _) => (Nil , count)
      case (x :: xs, None) => countSame(xs, Some(1, x))
      case (x :: xs, Some((c, y))) if (x == y) => countSame(xs, Some(c + 1, y))
      case _ => (list, count)
    }

    countSame(list, None) match {
      case (_, None) => Nil
      case (list, count) => count.get :: encode(list)
    }
  }

  /**
   * P11 (*) Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
   *
   * Example:
   *   scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *   res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   */
  def encodeModified[A](list: List[A]): List[Any] = {
    @tailrec
    def countSame(list: List[A], count: Option[(Int, A)]): (List[A], Option[(Int, A)]) = (list, count) match {
      case (Nil, _) => (Nil , count)
      case (x :: xs, None) => countSame(xs, Some(1, x))
      case (x :: xs, Some((c, y))) if (x == y) => countSame(xs, Some(c + 1, y))
      case _ => (list, count)
    }

    countSame(list, None) match {
      case (_, None) => Nil
      case (list, count) if (count.get._1 == 1) => count.get._2 :: encodeModified(list)
      case (list, count) => count.get :: encodeModified(list)
    }
  }
}
