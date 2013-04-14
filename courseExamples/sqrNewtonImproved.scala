
package progfun.examples 

import scala.math._

/** 
 * Iteratively computes square root of x using Newton approximations
 *
 * @author LAFK
 **/
class sqrRootNewtonWayImproved {

  /** Now in a block so x is visible everywhere within **/
def sqrt(x : Double) : Double = {

  def sqrtIter(guess : Double) : Double = {
    if (isGoodEnough(guess)) guess 
    else sqrtIter(improve(guess))
  }

  def isGoodEnough(guess : Double) = {
    abs(guess * guess -x) / x  < 0.001
  }

  def improve(guess : Double) = {
    (guess + x / guess) / 2
  }

  sqrtIter(1.0, x)
  }

}
