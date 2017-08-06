package muhammedf.ninetynine

import org.scalatest.FunSuite
import Logic._

class LogicTest extends FunSuite{

  test("basic logical operations"){
    /**
      * Produces result list for given operation with following inputs:
      * 1. True  True
      * 2. True  False
      * 3. False True
      * 4. False False
      */
    def exec(func:(Boolean,Boolean)=>Boolean):List[Boolean]={
      val bools=List(true, false)
      for(b1<-bools;b2<-bools) yield func(b1,b2)
    }
    val andresults=true::false::false::false::Nil
    val nandresults=false::true::true::true::Nil
    val orresults=true::true::true::false::Nil
    val norresults=false::false::false::true::Nil
    val xorresults=false::true::true::false::Nil
    val implresults=true::false::true::true::Nil
    val equresults=true::false::false::true::Nil

    assert(exec(and)==andresults, "and operation")
    assert(exec(nand)==nandresults, "nand operation")
    assert(exec(or)==orresults, "or operation")
    assert(exec(nor)==norresults, "nor operation")
    assert(exec(xor)==xorresults, "xor operation")
    assert(exec(impl)==implresults, "impl operation")
    assert(exec(equ)==equresults, "equ operation")
    assert(not(true)==false && not(false)==true, "not operation")
  }

  test("P49 (**) Gray code."){
    assert(gray(1)==List("0","1"))
    assert(gray(2)==List("00", "01", "11", "10"))
    assert(gray(3)==List("000", "001", "011", "010", "110", "111", "101", "100"))
  }

  test("P50 (***) Huffman code."){
    val freqs=List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))
    val expected=List(("a","0"), ("b","101"), ("c","100"), ("d","111"), ("e","1101"), ("f","1100"))
    val result=huffman(freqs)
    assert(expected==result)
  }

}