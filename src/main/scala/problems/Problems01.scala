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
  def penultimate[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException
    case x :: Nil => throw new NoSuchElementException
    case x :: _ :: Nil => x
    case x :: xs => penultimate(xs)
  }
}
