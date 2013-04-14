package progfun.examples 

import scala.math._


class sqrRootNewtonWay {
  /** 
   * Iteratively computes square root of x using Newton approximations
   *
   * @author LAFK
   **/
 def sqrtIter(guess : Double, x : Double) : Double = {
    if (isGoodEnough(guess, x)) guess 
    else sqrtIter(improve(guess,x), x)
  }

  def isGoodEnough(guess : Double, x : Double) = {
    abs(guess * guess -x) / x  < 0.001
  }

  def improve(guess : Double, x : Double) = {
    (guess + x / guess) / 2
  }

  def sqrt(x : Double) : Double = sqrtIter(1.0, x)
}
