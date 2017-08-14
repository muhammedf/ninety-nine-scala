package muhammedf.ninetynine

sealed abstract class Tree[+T]
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = s"T(${value.toString}  ${left.toString} ${right.toString})"
}
case object End extends Tree[Nothing] {
  override def toString: String = "."
}
object Node {
  def apply[T](value: T):Node[T] = Node(value, End, End)
}