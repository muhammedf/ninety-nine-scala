package muhammedf.ninetynine

class Logical(b:Boolean){

  def and(op:Boolean):Boolean=Logic.and(b, op)
  def nand(op:Boolean):Boolean=Logic.nand(b, op)
  def or(op:Boolean):Boolean=Logic.or(b, op)
  def nor(op:Boolean):Boolean=Logic.nor(b, op)
  def xor(op:Boolean):Boolean=Logic.xor(b, op)
  def impl(op:Boolean):Boolean=Logic.impl(b, op)
  def equ(op:Boolean):Boolean=Logic.equ(b, op)
  def not:Boolean=Logic.not(b)
}

object Logic{

  implicit def boolToLogical(b:Boolean):Logical=new Logical(b)

  def and(op1:Boolean, op2:Boolean):Boolean=op1&op2
  def nand(op1:Boolean, op2:Boolean):Boolean=this not op1&op2
  def or(op1:Boolean, op2:Boolean):Boolean=op1|op2
  def nor(op1:Boolean, op2:Boolean):Boolean=this not op1|op2
  def xor(op1:Boolean, op2:Boolean):Boolean=op1^op2
  def impl(op1:Boolean, op2:Boolean):Boolean=or(this not op1, op2)
  def equ(op1:Boolean, op2:Boolean):Boolean=and(impl(op1,op2), impl(op2, op1))
  def not(op:Boolean):Boolean=(!op)

  /**
    * P46 (**) Truth tables for logical expressions.
    * P47 (*) Truth tables for logical expressions (2).
    *
    * @param func
    */
  def table2(func:(Boolean, Boolean)=>Boolean)={
    val bools=List(true, false)
    Console println "A".padTo(10, ' ')+"B".padTo(10, ' ')+"result"
    for(b1<-bools;b2<-bools) Console println b1.toString.padTo(10, ' ')+b2.toString.padTo(10, ' ')+func(b1,b2).toString
  }

  /**
    * P49 (**) Gray code.
    *
    * @param n
    * @return
    */
  def gray(n: Int):List[String]={
    grays(n)
  }

  private lazy val grays:Stream[List[String]] = List("") #:: grays.map(l=>{
    l.map("0"+_):::l.reverse.map("1"+_)
  })

}