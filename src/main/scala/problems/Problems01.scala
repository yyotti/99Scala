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
}
