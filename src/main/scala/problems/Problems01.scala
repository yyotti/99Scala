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
    case x :: xs => last(xs)
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
    case x :: xs => penultimate(xs)
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
}
