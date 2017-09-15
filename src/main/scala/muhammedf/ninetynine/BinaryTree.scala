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
  def isMirrorOf[Y](tree: Tree[Y]):Boolean

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
  def isMirrorOf[Y](tree: Tree[Y]):Boolean = tree match {
    case Node(_, tleft, tright) => left.isMirrorOf(tright) && right.isMirrorOf(tleft)
    case End => false
  }
}
case object End extends Tree[Nothing] {
  override def toString: String = "."
  def nodeCount: Int = 0
  def isBalanced:Boolean = true
  def isSymmetric:Boolean = true
  def addValue[U >: Nothing <% Ordered[U]](x: U):Tree[U] = Node(x)
  def isMirrorOf[Y](tree: Tree[Y]):Boolean = tree == End
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
  def cBalanced[T](nodeCount: Int, value: T):List[Tree[T]] = nodeCount match {
    case 0 => End::Nil
    case 1 => Node(value)::Nil
    case n if n%2 == 1 => {
      val one = cBalanced((n-1)/2, value)
      one.flatMap(l => one.map(r => Node(value, l, r)))
    }
    case n if n%2 == 0 => {
      val one = cBalanced(n/2, value)
      val two = cBalanced((n-2)/2, value)
      one.flatMap(l => two.map(r => Node(value, l, r))) ::: one.flatMap(r => two.map(l => Node(value, l, r)))
    }
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

  /**
    * P58 (**) Generate-and-test paradigm.
    *
    * @param nodeCount
    * @param value
    * @tparam T
    * @return
    */
  def symmetricBalancedTrees[T](nodeCount:Int, value:T):List[Tree[T]]={
    Tree.cBalanced(nodeCount, value).filter(_.isSymmetric)
  }

  /**
    * P59 (**) Construct height-balanced binary trees.
    * @param height
    * @param value
    * @tparam T
    * @return
    */
  def hbalTrees[T](height:Int, value:T):List[Tree[T]]=height match {
    case 0 => End::Nil
    case 1 => Node(value)::Nil
    case n => {
      val oneless = hbalTrees(n-1, value);
      val twoless = hbalTrees(n-2, value);
      oneless.flatMap(o => twoless.flatMap(t => Node(value, o, t)::Node(value, t, o)::Nil)):::oneless.flatMap(o1 => oneless.map(o2 => Node(value, o1, o2)))
    }
  }

}