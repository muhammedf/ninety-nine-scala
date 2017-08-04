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

  }

}