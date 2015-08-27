package problems

package arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

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
     * P31 (**) Determine whether a given integer number is prime.
     *
     * scala> 7.isPrime
     * res0: Boolean = true
     */
    def isPrime: Boolean = ???
  }

  object S99Int {
    import scala.language.implicitConversions
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  }
}

object Arithmetic {
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
}
