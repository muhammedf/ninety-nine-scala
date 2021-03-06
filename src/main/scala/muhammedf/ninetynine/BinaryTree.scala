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

  /**
    * P61 (*) Count the leaves of a binary tree.
    *
    * @return
    */
  def leafCount:Int

  /**
    * 61A (*) Collect the leaves of a binary tree in a list.
    *
    * @return
    */
  def leafList:List[T]

  /**
    * P62 (*) Collect the internal nodes of a binary tree in a list.
    *
    * @return
    */
  def internalList:List[T]

  /**
    * P62B (*) Collect the nodes at a given level in a list.
    *
    * @param level
    * @return
    */
  def atLevel(level:Int):List[T]

  /**
    * P64 (**) Layout a binary tree (1).
    *
    * @return
    */
  def layoutBinaryTree:Tree[T]

  def layoutBinaryTree(parentX:Int, parentY:Int, symbol: Symbol):Tree[T]
}

abstract class Nod[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]{
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
  lazy val leafCount:Int = this match {
    case Node(_, End, End) => 1
    case _ => left.leafCount + right.leafCount
  }
  def leafList:List[T] = (left, right) match {
    case (End, End) => value::Nil
    case _ => left.leafList:::right.leafList
  }
  def internalList:List[T] = (left, right) match {
    case (End, End) => Nil
    case _ => value::left.internalList:::right.internalList
  }

  def atLevel(level: Int): List[T] = level match {
    case 1 => value::Nil
    case _ => left.atLevel(level-1):::right.atLevel(level-1)
  }

  def layoutBinaryTree(parentX:Int, parentY:Int, symbol: Symbol):PositionedNode[T] = symbol match {
    case 'leftie => {
      val x = parentX - 1 - right.nodeCount
      val y = parentY + 1
      PositionedNode(value, left.layoutBinaryTree(x, y, 'leftie), right.layoutBinaryTree(x, y, 'rightie), x, y)
    }
    case 'rightie =>{
      val x = parentX + 1 + left.nodeCount
      val y = parentY + 1
      PositionedNode(value, left.layoutBinaryTree(x, y, 'leftie), right.layoutBinaryTree(x, y, 'rightie), x, y)
    }
    case 'root => {
      val x = left.nodeCount + 1
      val y = 1
      PositionedNode(value, left.layoutBinaryTree(x, y, 'leftie), right.layoutBinaryTree(x, y, 'rightie), x, y)
    }
  }

  def layoutBinaryTree: Tree[T] = layoutBinaryTree(0, 0, 'root)

}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Nod[T](value, left, right)

case object End extends Tree[Nothing] {
  override def toString: String = "."
  def nodeCount: Int = 0
  def isBalanced:Boolean = true
  def isSymmetric:Boolean = true
  def addValue[U >: Nothing <% Ordered[U]](x: U):Tree[U] = Node(x)
  def isMirrorOf[Y](tree: Tree[Y]):Boolean = tree == End
  def leafCount:Int = 0
  def leafList:List[Nothing] = Nil
  def internalList:List[Nothing] = Nil
  def atLevel(level: Int): List[Nothing] = Nil
  def layoutBinaryTree = End
  def layoutBinaryTree(parentX: Int, parentY: Int, symbol: Symbol) = End
}
object Node {
  def apply[T](value: T):Node[T] = Node(value, End, End)
}

case class PositionedNode[+T](val value: T, val left: Tree[T], val right: Tree[T], x: Int, y: Int) extends Nod[T](value, left, right) {
  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
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

  def minHbalNodes(height:Int):Int = height match {
    case 0 => 0
    case 1 => 1
    case _ => 1 + minHbalNodes(height-1) + minHbalNodes(height-2)
  }

  def maxHbalHeight(nodeCount:Int):Int = {
    val minHeight = minHbalHeight(nodeCount)
    if(minHbalNodes(minHeight+1)<=nodeCount) minHeight+1 else minHeight
  }

  def minHbalHeight(nodeCount:Int):Int = (Math.log(nodeCount+1)/Math.log(2)).ceil.toInt

  def maxHbalNodes(height:Int):Int = Math.pow(2, height).toInt - 1

  /**
    *P60 (**) Construct height-balanced binary trees with a given number of nodes.
    *
    * @param nodeCount
    * @param value
    * @tparam T
    * @return
    */
  def hbalTreesWithNodes[T](nodeCount:Int, value:T):List[Tree[T]] = {
    (hbalTrees(minHbalHeight(nodeCount), value):::hbalTrees(maxHbalHeight(nodeCount), value))
      .filter(_.nodeCount==nodeCount)
  }

  /**
    *
    * @param nodeCount
    * @return the side of the last node (by complete binary tree)
    *         1 => root
    *         2 => left
    *         3 => right
    */
  def lastNodeAtWhichSide(nodeCount:Int):Int = {
    require(nodeCount>=0, "node count must be zero or greater than zero")
    nodeCount match {
      case n if n<4 => n
      case n if n%2==0 => lastNodeAtWhichSide(n/2);
      case n if n%2==1 => lastNodeAtWhichSide((n-1)/2)
    }
  }

  /**
    * P63 (**) Construct a complete binary tree.
    *
    * @param nodeCount
    * @param value
    * @tparam T
    * @return
    */
  def completeBinaryTree[T](nodeCount:Int, value:T):Tree[T] = lastNodeAtWhichSide(nodeCount) match {
    case 0 => End
    case 1 => Node(value)
    case 2 => {
      val height = minHbalHeight(nodeCount)
      val rightTreeHeight = height-2
      val rightTreeNodeCount = maxHbalNodes(rightTreeHeight)
      val leftTreeNodeCount = nodeCount - (rightTreeHeight + 1)
      Node(value, completeBinaryTree(leftTreeNodeCount, value), completeBinaryTree(rightTreeNodeCount, value))
    }
    case 3 => {
      val heiht = minHbalHeight(nodeCount)
      val leftTreeHeight = heiht-1
      val leftTreeNodeCount = maxHbalNodes(leftTreeHeight)
      val rightTreeNodeCount = nodeCount - (leftTreeNodeCount + 1)
      Node(value, completeBinaryTree(leftTreeNodeCount, value), completeBinaryTree(rightTreeNodeCount, value))
    }
  }
}