package muhammedf.ninetynine

sealed abstract class Tree[+T]{
  def nodeCount:Int
  def isBalanced:Boolean

  /**
    * P56 (**) Symmetric binary trees.
    *
    * @return
    */
  def isSymmetric:Boolean
  def isMirrorOf[Y](tree: Tree[Y]):Boolean = Tree.binFromTree(this) == Tree.binFromTree(tree)

  /**
    * P57 (**) Binary search trees (dictionaries).
    *
    * @param x
    * @tparam U
    * @return
    */
  def addValue[U >: T <% Ordered[U]](x: U):Tree[U]
}
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = s"T(${value.toString}  ${left.toString} ${right.toString})"
  lazy val isBalanced:Boolean = Math.abs(left.nodeCount - right.nodeCount)<=1 && left.isBalanced && right.isBalanced
  lazy val nodeCount:Int = 1 + left.nodeCount + right.nodeCount
  lazy val isSymmetric:Boolean = left.isMirrorOf(right)
  def addValue[U >: T <% Ordered[U]](x: U):Tree[U] = {
    if(x>=value) Node(value, left, right.addValue(x))
    else Node(value, left.addValue(x), right)
  }
}
case object End extends Tree[Nothing] {
  override def toString: String = "."
  def nodeCount: Int = 0
  def isBalanced:Boolean = true
  def isSymmetric:Boolean = true
  def addValue[U >: Nothing <% Ordered[U]](x: U):Tree[U] = Node(x)
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

  /**
    * P57 (**) Binary search trees (dictionaries).
    *
    * @param list
    * @tparam T
    * @return
    */
  def fromList[T <% Ordered[T]](list: List[T]):Tree[T]={
    def fl(liste:List[T], tree: Tree[T]):Tree[T]=liste match {
      case last::Nil => tree.addValue(last)
      case head::tail => fl(tail, tree.addValue(head))
    }
    fl(list, End)
  }

}