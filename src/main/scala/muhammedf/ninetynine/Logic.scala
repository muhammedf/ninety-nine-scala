package muhammedf.ninetynine

object Logic{

  def and(op1:Boolean, op2:Boolean):Boolean=op1&op2
  def nand(op1:Boolean, op2:Boolean):Boolean=this not op1&op2
  def or(op1:Boolean, op2:Boolean):Boolean=op1|op2
  def nor(op1:Boolean, op2:Boolean):Boolean=this not op1|op2
  def xor(op1:Boolean, op2:Boolean):Boolean=op1^op2
  def impl(op1:Boolean, op2:Boolean):Boolean=or(this not op1, op2)
  def equ(op1:Boolean, op2:Boolean):Boolean=and(impl(op1,op2), impl(op2, op1))
  def not(op:Boolean):Boolean=(!op)

}