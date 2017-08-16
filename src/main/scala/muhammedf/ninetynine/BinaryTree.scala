package muhammedf.ninetynine

sealed abstract class Tree[+T]{
  def nodeCount:Int
  def isBalanced:Boolean
  def isSymmetric:Boolean
  def isMirrorOf[Y](tree: Tree[Y]):Boolean = Tree.binFromTree(this) == Tree.binFromTree(tree)
}
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = s"T(${value.toString}  ${left.toString} ${right.toString})"
  lazy val isBalanced:Boolean = Math.abs(left.nodeCount - right.nodeCount)<=1 && left.isBalanced && right.isBalanced
  lazy val nodeCount:Int = 1 + left.nodeCount + right.nodeCount
  lazy val isSymmetric:Boolean = left.isMirrorOf(right)

}
case object End extends Tree[Nothing] {
  override def toString: String = "."
  def nodeCount: Int = 0
  def isBalanced:Boolean = true
  def isSymmetric:Boolean = true
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
      case _ => throw new IllegalArgumentException("Only 1 and 0 are allowed.")
    }
    tfb()
  }

  def binFromTree[T](tree: Tree[T]):String=tree match {
    case End => "0"
    case Node(_, left, right) => "1" + binFromTree(left) + binFromTree(right)
  }

}