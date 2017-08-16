package muhammedf.ninetynine

sealed abstract class Tree[+T]{
  def nodeCount:Int
  def isBalanced:Boolean
}
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = s"T(${value.toString}  ${left.toString} ${right.toString})"
  lazy val isBalanced:Boolean = Math.abs(left.nodeCount - right.nodeCount)<=1 && left.isBalanced && right.isBalanced
  lazy val nodeCount:Int = 1 + left.nodeCount + right.nodeCount
}
case object End extends Tree[Nothing] {
  override def toString: String = "."
  def nodeCount: Int = 0
  def isBalanced:Boolean = true
}
object Node {
  def apply[T](value: T):Node[T] = Node(value, End, End)
}

object Tree {

  /**
    * P55 (**) Construct completely balanced binary trees.
    *
    * @param nodeCount
    * @param value
    * @tparam T
    * @return
    */
  def cBalanced[T](nodeCount: Int, value: T):List[Tree[T]]={
    if(nodeCount==0) End::Nil
    else
      ("1"*(nodeCount-1)+"0"*nodeCount)
      .permutations
      .map("1"+_+"0")
      .map(treeFromBin(_, value))
      .filter(_.nodeCount==nodeCount)
      .filter(_.isBalanced).toList
  }

  def treeFromBin[T](bin:String, put:T):Tree[T]={
    var binn=bin.toList
    def tfb():Tree[T]=binn match {
      case '1' :: rem => binn = rem; Node(put, tfb(), tfb())
      case '0' :: rem => binn = rem; End
      case Nil => End
    }
    if(!binn.forall(b => b=='1' || b=='0')) throw new IllegalArgumentException("Only 1 and 0 are allowed.")
    tfb()
  }

}