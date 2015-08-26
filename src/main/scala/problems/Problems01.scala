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
  def compress[A](list: List[A]): List[A] = ???
}
