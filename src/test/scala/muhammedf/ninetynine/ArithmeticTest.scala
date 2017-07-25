package muhammedf.ninetynine

import org.scalatest.FunSuite
import Arithmetic.intToRichie

/**
  * Created by muhammedf on 23.07.2017.
  */
class ArithmeticTest extends FunSuite{

  test("P31 (**) Determine whether a given integer number is prime."){
    assert(7.isPrime==true)
    assert(49.isPrime==false)
  }

  test("P32 (**) Determine the greatest common divisor of two positive integer numbers."){
    assert(Arithmetic.gcd(36,63)==9)
  }

  test("P33 (*) Determine whether two positive integer numbers are coprime."){
    assert(35.isCoprimeTo(64)==true)
    assert(35.isCoprimeTo(65)==false)
  }

  test("P34 (**) Calculate Euler's totient function phi(m)."){
    assert(10.totient==4)
  }

  test("P35 (**) Determine the prime factors of a given positive integer."){
    assert(315.primeFactors==List(3, 3, 5, 7))
  }

  test("P36 (**) Determine the prime factors of a given positive integer (2)."){
    assert(315.primeFactorMultiplicity==List((3,2), (5,1), (7,1)))
  }
}
