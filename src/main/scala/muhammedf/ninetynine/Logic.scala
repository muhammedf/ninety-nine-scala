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

  /**
    * P50 (***) Huffman code.
    *
    * @param freqs
    * @return
    */
  def huffman(freqs: List[(String, Int)]):List[(String, String)]={

    case class Node(val left: Option[Node] = None, val right: Option[Node] = None, val value: Option[String] = None)

    /**
      * Puts the node that have small freq to the left side.
      * @param list
      * @return
      */
    def produceTree(list: List[(Int, Node)]):Node=list match {
      case t1 :: t2 :: Nil => Node(Some(t1._2), Some(t2._2))
      case t1 :: t2 :: rem => produceTree(((t1._1+t2._1, Node(Some(t1._2), Some(t2._2)))::rem).sortWith(_._1<_._1))
    }

    def produceCodes(node: Node, curcode: String):List[(String, String)]=node match {
      case Node(None, None, x) => (x.get, curcode) :: Nil
      case Node(Some(x), Some(y), None) => produceCodes(x, curcode+"0") ::: produceCodes(y, curcode+"1")
    }

    val nodes=freqs.map(t=>(t._2, new Node(value = Some(t._1)))).sortWith(_._1<_._1)
    val root=produceTree(nodes)
    val codes=produceCodes(root, "")
    val order=freqs.unzip._1
    codes.sortWith((t1, t2)=>order.indexOf(t1._1)<order.indexOf(t2._1))
  }

}