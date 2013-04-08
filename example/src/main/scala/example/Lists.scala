package example

import common._

object Lists {
  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
  def sum(xs: List[Int]): Int = {
    xs.sum
  }  

  /**
   * Now when tests told me how summing the lists works, time to implement it with recursion.
   * Hence the R in function name.
   *
   * @author Tomasz Borek
   */
  def rsum(xs : List[Int]) : Int = {
    var res : Int = 0;
    if (xs.isEmpty) { 
      return 0; // corner case
    }
    else {
      res += xs.head
      res += rsum(xs.tail) // if I return here it's a Unit
      res
      }
  }

  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
  def max(xs: List[Int]): Int = {
    try {
      xs.max
    } catch {  // somehow I completely missed this untill I started implementing rsum!
      case e : UnsupportedOperationException => throw new NoSuchElementException (e.getMessage())
    }
  }

  /**
   * Same as rsum, but for maximum.
   *
   * @author Tomasz Borek
   **/
  def rmax(xs : List[Int]) : Int = {
    // will throw IllegalArgument. require (!xs.isEmpty, "Can't have maximum on empty list")
    if (xs.isEmpty) {
      throw new NoSuchElementException("Empty list - max not supported")
    } else {
      rmaxOnNonEmptyList(xs.head, xs.tail)
    }
  }

  def rmaxOnNonEmptyList(currentMax : Int, rest : List[Int]) : Int = {
    var aMax = currentMax
    if (rest.isEmpty) {
      return aMax
    } else if (aMax < rest.head) {
      aMax = rmaxOnNonEmptyList(rest.head, rest.tail)
      aMax
    } else {
      aMax = rmaxOnNonEmptyList(aMax, rest.tail)
      aMax
    }
  }
}
