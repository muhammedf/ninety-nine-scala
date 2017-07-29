package muhammedf.ninetynine

import Arithmetic.intToRichie

/**
  * Created by muhammedf on 23.07.2017.
  */
class RichieInt(val x:Int) {

  /**
    * P31 (**) Determine whether a given integer number is prime.
    *
    * @return
    */
  def isPrime:Boolean=(2 to Math.sqrt(x).toInt).forall(x%_!=0)

  /**
    * P33 (*) Determine whether two positive integer numbers are coprime.
    *
    * @param y
    * @return
    */
  def isCoprimeTo(y:Int):Boolean=Arithmetic.gcd(x, y)==1

  /**
    * P34 (**) Calculate Euler's totient function phi(m).
    *
    * @return
    */
  def totient:Int=(1 to x).filter(this.isCoprimeTo(_)).size

  /**
    * P35 (**) Determine the prime factors of a given positive integer.
    *
    * @return
    */
  def primeFactors:List[Int]={
    def factors(currentPrime:Int, currentNumber:Int):List[Int]=currentNumber%currentPrime==0 match {
      case true if currentNumber/currentPrime==1 => currentPrime :: Nil
      case true => currentPrime :: factors(currentPrime, currentNumber/currentPrime)
      case false => factors(Arithmetic.nextPrime(currentPrime), currentNumber)
    }
    factors(2, x)
  }

  /**
    * P36 (**) Determine the prime factors of a given positive integer (2).
    *
    * @return
    */
  def primeFactorMultiplicity:List[(Int,Int)]=QList.encode(this.primeFactors).map(_.swap)

  /**
    * P37 (**) Calculate Euler's totient function phi(m) (improved).
    *
    * @return
    */
  def totientImproved:Int=this.primeFactorMultiplicity.map((t:(Int,Int))=>(t._1-1)*Math.pow(t._1, t._2-1).toInt).product

  /**
    * P40 (**) Goldbach's conjecture.
    *
    * @return
    */
  def goldbach:(Int, Int)={
    def gold(cur:Int):Int={
      if((x-cur).isPrime) cur
      else gold(Arithmetic.nextPrime(cur))
    }
    assert(x%2==0 && x>2)
    val op1=gold(2)
    (op1, x-op1)
  }
}

object Arithmetic {
  implicit def intToRichie(x: Int): RichieInt = new RichieInt(x)

  /**
    * P32 (**) Determine the greatest common divisor of two positive integer numbers.
    *
    * @param num1
    * @param num2
    * @return
    */
  def gcd(num1: Int, num2: Int): Int = {
    if (num2 == 0) num1
    else gcd(num2, num1 % num2)
  }

  /**
    * P38 (*) Compare the two methods of calculating Euler's totient function.
    *
    * @param num
    */
  def testTotients(num: Int):Unit={
    def durationOf(func:()=>Any):Long={
      val cur=System.currentTimeMillis()
      func()
      System.currentTimeMillis()-cur
    }
    def printDuration(func:String, dur:Long):Unit={
      println(func+": "+dur+" ms")
    }
    printDuration("totient", durationOf(()=>num.totient))
    printDuration("totient improved", durationOf(()=>num.totientImproved))
  }

  def nextPrime(current:Int):Int= current match {
    case 2 => 3
    case _ if current%2==1 => if((current+2).isPrime) current+2 else nextPrime(current+2)
    case _ => if((current+1).isPrime) current+1 else nextPrime(current+1)
  }

  /**
    * P39 (*) A list of prime numbers.
    *
    * @param range
    * @return
    */
  def listPrimesinRange(range: Range):List[Int]={
    def collect(cur:Int):List[Int]={
      if(cur>range.end) Nil
      else cur::collect(nextPrime(cur))
    }
    collect(nextPrime(range.start-1))
  }

  def printSum(op1:Int, op2:Int):Unit={
    Console println op1+op2+" = "+op1+" + "+op2
  }

  /**
    * P41 (**) A list of Goldbach compositions. (a)
    *
    * @param range
    */
  def printGoldbachList(range: Range):Unit={
    range.filter(a=>a%2==0 && a>2).foreach{
      x =>
        val (op1, op2)=x.goldbach
        printSum(op1, op2)
    }
  }

  /**
    * P41 (**) A list of Goldbach compositions. (b)
    *
    * @param range
    * @param lowerLimit
    */
  def printGoldbachListLimited(range: Range, lowerLimit:Int):Unit={
    range.filter(a=>a%2==0 && a>2).map(_.goldbach).filter((t:(Int,Int))=>t._1>lowerLimit && t._2>lowerLimit).foreach((t:(Int,Int))=>printSum(t._1,t._2))
  }

}
