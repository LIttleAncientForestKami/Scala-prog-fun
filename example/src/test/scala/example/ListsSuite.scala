package example

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class implements a ScalaTest test suite for the methods in object
 * `Lists` that need to be implemented as part of this assignment. A test
 * suite is simply a collection of individual tests for some specific
 * component of a program.
 *
 * A test suite is created by defining a class which extends the type
 * `org.scalatest.FunSuite`. When running ScalaTest, it will automatically
 * find this class and execute all of its tests.
 *
 * Adding the `@RunWith` annotation enables the test suite to be executed
 * inside eclipse using the built-in JUnit test runner.
 *
 * You have two options for running this test suite:
 * 
 * - Start the sbt console and run the "test" command
 * - Right-click this file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class BasicSuite extends FunSuite {

  /**
   * Tests are written using the `test` operator which takes two arguments:
   *
   * - A description of the test. This description has to be unique, no two
   *   tests can have the same description.
   * - The test body, a piece of Scala code that implements the test
   *
   * The most common way to implement a test body is using the method `assert`
   * which tests that its argument evaluates to `true`. So one of the simplest
   * successful tests is the following:
   */
  test("one plus one is two")(assert(1 + 1 == 2))


  /**
   * In Scala, it is allowed to pass an argument to a method using the block
   * syntax, i.e. `{ argument }` instead of parentheses `(argument)`.
   *
   * This allows tests to be written in a more readable manner:
   */
  test("one plus one is three?") {
    assert(1 + 2 == 3) // This assertion fails! Go ahead and fix it.
  }


  /**
   * One problem with the previous (failing) test is that ScalaTest will
   * only tell you that a test failed, but it will not tell you what was
   * the reason for the failure. The output looks like this:
   *
   * {{{
   *    [info] - one plus one is three? *** FAILED ***
   * }}}
   *
   * This situation can be improved by using a special equality operator
   * `===` instead of `==` (this is only possible in ScalaTest). So if you
   * run the next test, ScalaTest will show the following output:
   *
   * {{{
   *    [info] - details why one plus one is not three *** FAILED ***
   *    [info]   2 did not equal 3 (ListsSuite.scala:67)
   * }}}
   *
   * We recommend to always use the `===` equality operator when writing tests.
   */
  test("details why one plus one is not three") {
    assert(1 + 2 === 3) // Fix me, please!
  }


  /**
   * In order to test the exceptional behavior of a methods, ScalaTest offers
   * the `intercept` operation.
   *
   * In the following example, we test the fact that the method `intNotZero`
   * throws an `IllegalArgumentException` if its argument is `0`.
   */
  test("intNotZero throws an exception if its argument is 0") {
    intercept[IllegalArgumentException] {
      intNotZero(0)
    }
  }

  def intNotZero(x: Int): Int = {
    if (x == 0) throw new IllegalArgumentException("zero is not allowed")
    else x
  }
}


class ListSumSuite extends FunSuite {
  /**
   * Now we finally write some tests for the list functions that have to be
   * implemented for this assignment. We fist import all members of the
   * `List` object.
   */ 
  import Lists._
  

  /**
   * We only provide two very basic tests for you. Write more tests to make
   * sure your `sum` and `max` methods work as expected.
   *
   * In particular, write tests for corner cases: negative numbers, zeros,
   * empty lists, lists with repeated elements, etc.
   *
   * It is allowed to have multiple `assert` statements inside one test,
   * however it is recommended to write an individual `test` statement for
   * every tested aspect of a method.
   *
   *
   * LAFK: I'll add other assertion for my rsum. No need to duplicate tests here,
   * since I explicitly want rsum to behave like List.sum does.
   *
   */
  test("sum of a few numbers") {
    assert(sum(List(1,2,0)) === 3)
    assert(rsum(List(1,2,0)) === 3)
  }
 
  test("sum of zeroes") {
    assert(sum(List(0,0,0,0)) === 0)
    assert(rsum(List(0,0,0,0)) === 0)
  }

  test("sum with one negative") {
    assert(sum(List(-5)) === -5)
    assert(rsum(List(-5)) === -5)
  }

  test("sum with many numbers, some negative") {
    assert(sum(List(1,34,56,-3,-17,9,11,23,678,-1001,-0,-0,-0, 9, -3, 23, 200)) === 20)
    assert(rsum(List(1,34,56,-3,-17,9,11,23,678,-1001,-0,-0,-0, 9, -3, 23, 200)) === 20)
  }

  test("sum with repetitive numbers") {
    assert(sum(List(-3,-3,-3,-3,-3,-3,-3,3,3,3,3,3,3,3)) === 0)
    assert(rsum(List(-3,-3,-3,-3,-3,-3,-3,3,3,3,3,3,3,3)) === 0)
  }

   test("empty list counts as 0") {
    assert(sum(List()) === 0)
    assert(rsum(List()) === 0)
  }

}

class ListMaxSuite extends FunSuite {

  import Lists._

  test("max of a few numbers") {
    assert(max(List(3, 7, 2)) === 7)
    assert(rmax(List(3, 7, 2)) === 7)
  }

  test("max with three numbers from big list - helper test") {
    assert(max(List(1,34,56)) === 56)
    assert(rmax(List(1,34,56)) === 56)
  }
  
  test("max of zeroes") {
    assert(max(List(0,0,0,0)) === 0)
    assert(rmax(List(0,0,0,0)) === 0)
  }

  test("max with one negative") {
    assert(max(List(-5)) === -5)
    assert(rmax(List(-5)) === -5)
  }

  test("max with many numbers, some negative") {
    assert(max(List(1,34,56,-3,-17,9,11,23,678,-1001,-0,-0,-0, 9, -3, 23, 200)) === 678)
    assert(rmax(List(1,34,56,-3,-17,9,11,23,678,-1001,-0,-0,-0, 9, -3, 23, 200)) === 678)
  }

  test("max with repetitive numbers") {
    assert(max(List(-3,-3,-3,-3,-3,-3,-3,3,3,3,3,3,3,3)) === 3)
    assert(rmax(List(-3,-3,-3,-3,-3,-3,-3,3,3,3,3,3,3,3)) === 3)
  }

  test("empty list cannot give maximum") {
    intercept [NoSuchElementException] {
      assert(max(List()) === "no matter, should throw")
    }
    intercept [NoSuchElementException] {
      assert(rmax(List()) === "no matter, should throw")
    }
  }
}
