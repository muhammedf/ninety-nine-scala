package muhammedf.ninetynine

import org.scalatest.FunSuite
/**
  * Created by muhammedf on 12.07.2017.
  */
class QListTest extends FunSuite{

  test("P01 (*) Find the last element of a list."){
    val list=List(1, 1, 2, 3, 5, 8)
    val expected=8
    val result=QList.last(list)
    assert(expected==result)
  }

  test("P02 (*) Find the last but one element of a list."){
    val list=List(1, 1, 2, 3, 5, 8)
    val expected=5
    val result=QList.penultimate(list)
    assert(expected==result)
  }

  test("P03 (*) Find the Kth element of a list."){
    val list=List(1, 1, 2, 3, 5, 8)
    val n=2
    val expected=2
    val result=QList.nth(n, list)
    assert(expected==result)
  }

  test("P04 (*) Find the number of elements of a list."){
    val list=List(1, 1, 2, 3, 5, 8)
    val expected=6
    val result=QList.length(list)
    assert(expected==result)
  }

  test("P05 (*) Reverse a list."){
    val list=List(1, 1, 2, 3, 5, 8)
    val expected=List(8, 5, 3, 2, 1, 1)
    val result=QList.reverse(list)
    assert(expected==result)
  }

  test("P06 (*) Find out whether a list is a palindrome."){
    val list1=List(1, 2, 3, 2, 1)
    val expected1=true
    val result1=QList.isPalindrome(list1)
    assert(expected1==result1)
    val list2=List(1, 2, 3, 4, 1)
    val expected2=false
    val result2=QList.isPalindrome(list2)
    assert(expected2==result2)
  }

  test("P07 (**) Flatten a nested list structure."){
    val list=List(List(1, 1), 2, List(3, List(5, 8)))
    val expected=List(1, 1, 2, 3, 5, 8)
    val result = QList.flatten(list)
    assert(expected==result)
  }

  test("P08 (**) Eliminate consecutive duplicates of list elements."){
    val list=List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val expected=List('a, 'b, 'c, 'a, 'd, 'e)
    val result=QList.compress(list)
    assert(expected==result)
  }

  test("P09 (**) Pack consecutive duplicates of list elements into sublists."){
    val list=List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val expected=List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    val result=QList.pack(list)
    assert(expected==result)
  }

  test("P10 (*) Run-length encoding of a list."){
    val list=List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val expected=List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    val result=QList.encode(QList.pack(list))
    assert(expected==result)
  }

  test("P11 (*) Modified run-length encoding."){
    val list=List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val expected=List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    val result=QList.encodeModified(QList.pack(list))
    assert(expected==result)
  }

  test("P12 (**) Decode a run-length encoded list."){
    val list=List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    val expected=List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val result=QList.decode(list)
    assert(expected==result)
  }

  test("P13 (**) Run-length encoding of a list (direct solution)."){
    val list=List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val expected=List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    val result=QList.encodeDirect(list)
    assert(expected==result)
  }

  test("P14 (*) Duplicate the elements of a list."){
    val list=List('a, 'b, 'c, 'c, 'd)
    val expected=List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    val result=QList.duplicate(list)
    assert(expected==result)
  }

  test("P15 (**) Duplicate the elements of a list a given number of times."){
    val list=List('a, 'b, 'c, 'c, 'd)
    val times=3
    val expected=List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    val result=QList.duplicateN(times, list)
    assert(expected==result)
  }

  test("P16 (**) Drop every Nth element from a list."){
    val list=List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val n=3
    val expected=List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    val result=QList.drop(n, list)
    assert(expected==result)
  }

  test("P17 (*) Split a list into two parts."){
    val list=List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val firstLength=3
    val expected=(List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    val result=QList.split(firstLength, list)
    assert(expected==result)
  }

  test("P18 (**) Extract a slice from a list."){
    val list=List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val (beginIndex, endIndex)=(3,7)
    val expected=List('d, 'e, 'f, 'g)
    val result=QList.slice(beginIndex, endIndex, list)
    assert(expected==result)
  }

  test("P19 (**) Rotate a list N places to the left."){
    val list=List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val (angle1, angle2)=(3,-2)
    val (expected1, expected2)=(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c),List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    val result1=QList.rotate(angle1, list)
    val result2=QList.rotate(angle2, list)
    assert(expected1==result1)
    assert(expected2==result2)
  }

  test("P19 (**) Rotate a list N places to the left. (For Second Solution)"){
    val list=List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val (angle1, angle2)=(3,-2)
    val (expected1, expected2)=(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c),List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    val result1=QList.rotate2(angle1, list)
    val result2=QList.rotate2(angle2, list)
    assert(expected1==result1)
    assert(expected2==result2)
  }

  test("P20 (*) Remove the Kth element from a list."){
    val list=List('a, 'b, 'c, 'd)
    val index=1
    val expected=(List('a, 'c, 'd),'b)
    val result=QList.removeAt(index, list)
    assert(expected==result)
  }

  test("P21 (*) Insert an element at a given position into a list."){
    val list=List('a, 'b, 'c, 'd)
    val (newelement, index)=('new, 1)
    val expected=List('a, 'new, 'b, 'c, 'd)
    val result=QList.insertAt(newelement, index, list)
    assert(expected==result)
  }

  test("P22 (*) Create a list containing all integers within a given range."){
    val (begin, end)=(4,9)
    val expected=List(4, 5, 6, 7, 8, 9)
    val result=QList.range(begin, end)
    assert(expected==result)
  }

  test("P23 (**) Extract a given number of randomly selected elements from a list."){
    val list=List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    val number=3
    val result=QList.randomSelect(number, list)
    assert(result.size==number)
    assert(list.forall(list.contains(_)))
  }

  test("P24 (*) Lotto: Draw N different random numbers from the set 1..M."){
    val (number, bound)=(6,49)
    val result=QList.lotto(number, bound)
    assert(result.size==number)
    assert(result.forall(n=> n<=bound && n>0))
  }

  test("P25 (*) Generate a random permutation of the elements of a list."){
    val list=List('a, 'b, 'c, 'd, 'e, 'f)
    val result=QList.randomPermute(list)
    assert(list.size==result.size)
    assert(result.forall(list.contains(_)))
  }

  def computeCombinationSize(n:Int, listSize: Int):Int=List.range(listSize-n+1, listSize+1).product/List.range(1, n + 1).product

  test("P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list."){
    val list=List('a, 'b, 'c, 'd, 'e, 'f)
    val size=3
    val comSize=computeCombinationSize(size, list.size)
    val result=QList.combinations(size, list)
    val sortedSet = result.map(r=>r.sortWith(_.toString < _.toString)).toSet
    assert(sortedSet.forall(_.size==size))
    assert(sortedSet.forall(_.forall(list.contains(_))))
    assert(sortedSet.size==comSize)
  }

  def computeGroupCombinationSize(sizes:List[Int], listSize: Int):Int=sizes match {
    case Nil => 1
    case head::tail => computeCombinationSize(head, listSize)*computeGroupCombinationSize(tail, listSize-head)
  }

  test("P27 (**) Group the elements of a set into disjoint subsets. (a) (2,3,4)"){
    val list=List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val sizes=List(2,3,4)
    val result=QList.group3(list)
    val comSize=computeGroupCombinationSize(sizes, list.size)
    assert(result.size==comSize)
    assert(result.forall(_.flatten.toSet.size==list.size))
  }

  test("P27 (**) Group the elements of a set into disjoint subsets. (b)"){
    val list=List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val sizes=List(2,3,2,2)
    val result=QList.group(sizes, list)
    val comSize=computeGroupCombinationSize(sizes, list.size)
    assert(result.size==comSize)
    assert(result.forall(_.flatten.toSet.size==list.size))
  }

  test("P28 (**) Sorting a list of lists according to length of sublists. (a)"){
    val list=List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    val expected=List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    val result=QList.lsort(list)
    assert(expected==result)
  }

  test("P28 (**) Sorting a list of lists according to length of sublists. (b)"){
    val list=List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    val expected=List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
    val result=QList.lsortFreq(list)
    assert(expected==result)
  }
}
