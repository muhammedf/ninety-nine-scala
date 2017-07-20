package muhammedf.ninetynine

import java.util.Random
/**
  * Created by muhammedf on 12.07.2017.
  */
object QList {

  /**
    * P01 (*) Find the last element of a list.
    *
    * @param list
    * @tparam T
    * @return
    */
  def last[T](list: List[T]): T = list.last

  /**
    * P02 (*) Find the last but one element of a list.
    *
    * @param list
    * @tparam T
    * @return
    */
  def penultimate[T](list: List[T]): T = list.init.last

  /**
    * P03 (*) Find the Kth element of a list.
    *
    * @param n
    * @param list
    * @tparam T
    * @return
    */
  def nth[T](n: Int, list: List[T]): T = list(n)

  /**
    * P04 (*) Find the number of elements of a list.
    *
    * @param list
    * @return
    */
  def length(list: List[Any]): Int = list.size

  /**
    * P05 (*) Reverse a list.
    *
    * @param list
    * @tparam T
    * @return
    */
  def reverse[T](list: List[T]): List[T] = list.reverse

  /**
    * P06 (*) Find out whether a list is a palindrome.
    *
    * @param list
    * @return
    */
  def isPalindrome(list: List[Any]): Boolean = list == list.reverse

  /**
    * P07 (**) Flatten a nested list structure.
    *
    * @param list
    * @return
    */
  def flatten(list: List[Any]): List[Any] = list flatMap {
    case a: List[_] => flatten(a)
    case b => List(b)
  }

  /**
    * P08 (**) Eliminate consecutive duplicates of list elements.
    *
    * @param list
    * @tparam T
    * @return
    */
  def compress[T](list: List[T]): List[T] = {
    list.sliding(2).collect {
      case List(a, b) if a != b => a
    }.toList ::: List(list.last)
  }

  /**
    * P09 (**) Pack consecutive duplicates of list elements into sublists.
    *
    * @param list
    * @tparam T
    * @return
    */
  def pack[T](list: List[T]): List[List[T]] = {
    def pack(currentList: List[T], remainingList: List[T], packedList: List[List[T]]): List[List[T]] = {
      currentList match {
        case headcur :: tailcur => remainingList match {
          case headrem :: tailrem if headcur == headrem => pack(headcur :: currentList, remainingList.tail, packedList)
          case headrem :: tailrem if headcur != headrem => pack(remainingList.head :: Nil, remainingList.tail, currentList :: packedList)
          case Nil => currentList :: packedList
        }
      }
    }
    pack(List(list.head), list.tail, Nil).reverse
  }

  /**
    * P10 (*) Run-length encoding of a list.
    *
    * @param list
    * @tparam T
    * @return
    */
  def encode[T](list: List[List[T]]): List[(Int, T)] = for (l <- list) yield (l.size, l.head)

  /**
    * P11 (*) Modified run-length encoding.
    *
    * @param list
    * @tparam T
    * @return
    */
  def encodeModified[T](list: List[List[T]]): List[Any] = list map {
    l =>
      l match {
        case head :: Nil => head
        case head :: tail => (l.size, head)
      }
  }

  /**
    * P12 (**) Decode a run-length encoded list.
    *
    * @param list
    * @tparam T
    * @return
    */
  def decode[T](list: List[(Int, T)]): List[T] = {
    def decode_tupple(tupple: (Int, T)): List[T] = {
      val (size, element) = tupple
      List.fill(size)(element)
    }

    list match {
      case head :: Nil => decode_tupple(head)
      case head :: tail => decode_tupple(list.head) ::: decode(tail)
    }
  }

  /**
    * P13 (**) Run-length encoding of a list (direct solution).
    *
    * @param list
    * @tparam T
    * @return
    */
  def encodeDirect[T](list: List[T]): List[(Int, T)] = list match {
    case Nil => Nil
    case _ => {
      val (cur, rem) = list span (_ == list.head)
      (cur.size, cur.head) :: encodeDirect(rem)
    }
  }

  /**
    * P14 (*) Duplicate the elements of a list.
    *
    * @param list
    * @tparam T
    * @return
    */
  def duplicate[T](list: List[T]): List[T] = list map (l => List.fill(2)(l)) flatten

  /**
    * P15 (**) Duplicate the elements of a list a given number of times.
    *
    * @param n
    * @param list
    * @tparam T
    * @return
    */
  def duplicateN[T](n: Int, list: List[T]): List[T] = list map (l => List.fill(n)(l)) flatten

  /**
    * P16 (**) Drop every Nth element from a list.
    *
    * @param n
    * @param list
    * @tparam T
    * @return
    */
  def drop[T](n: Int, list: List[T]): List[T] = list.zipWithIndex.filter {
    case (l, i) => (i + 1) % n != 0
  }.unzip._1

  /**
    * P17 (*) Split a list into two parts.
    *
    * @param length
    * @param list
    * @tparam T
    * @return
    */
  def split[T](length: Int, list: List[T]): (List[T], List[T]) = list splitAt (length)

  /**
    * P18 (**) Extract a slice from a list.
    *
    * @param first
    * @param last
    * @param list
    * @tparam T
    * @return
    */
  def slice[T](first: Int, last: Int, list: List[T]): List[T] = list.splitAt(first)._2.splitAt(last - first)._1

  /**
    * P19 (**) Rotate a list N places to the left.
    *
    * @param angle
    * @param list
    * @tparam T
    * @return
    */
  def rotate[T](angle: Int, list: List[T]): List[T] = list match {
    case Nil => Nil
    case _ if angle > 0 => rotate(angle - 1, list.filter(list.indexOf(_) != angle - 1)) ::: List(list(angle - 1))
    case _ if angle < 0 => list(list.length + angle) :: rotate(angle + 1, list.filter(list.indexOf(_) != list.length + angle))
    case _ => list
  }

  /**
    * P19 (**) Rotate a list N places to the left.
    * (Second Solution)
    *
    * @param angle
    * @param list
    * @tparam T
    * @return
    */
  def rotate2[T](angle: Int, list: List[T]): List[T] = {
    def lengthComplement(angle: Int, length: Int): Int = if (angle >= 0) angle else length + angle

    val index = lengthComplement(angle, list.length)
    val (head, tail) = list.splitAt(index)
    tail ::: head
  }

  /**
    * P20 (*) Remove the Kth element from a list.
    *
    * @param index
    * @param list
    * @tparam T
    * @return
    */
  def removeAt[T](index: Int, list: List[T]): (List[T], T) = {
    val (element, remList) = list.partition(list.indexOf(_) == index)
    (remList, element.head)
  }

  /**
    * P21 (*) Insert an element at a given position into a list.
    *
    * @param element
    * @param index
    * @param list
    * @tparam T
    * @return
    */
  def insertAt[T](element: T, index: Int, list: List[T]): List[T] = {
    val (headList, tailList) = list.splitAt(index)
    headList ::: element :: tailList
  }

  /**
    * P22 (*) Create a list containing all integers within a given range.
    *
    * @param begin
    * @param end
    * @return
    */
  def range(begin: Int, end: Int): List[Int] = List.range(begin, end + 1)

  /**
    * P23 (**) Extract a given number of randomly selected elements from a list.
    *
    * @param number
    * @param list
    * @tparam T
    * @return
    */
  def randomSelect[T](number: Int, list: List[T]): List[T] = list match {
    case Nil => Nil
    case _ if number < 1 => Nil
    case _ if number > list.size => throw new NoSuchElementException
    case _ => {
      val random = new Random
      val index = random.nextInt(list.size)
      val (remList, element) = removeAt(index, list)
      element :: randomSelect(number - 1, remList)
    }
  }

  /**
    * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
    *
    * @param number
    * @param bound
    * @return
    */
  def lotto(number: Int, bound: Int): List[Int] = randomSelect(number, List.range(1, bound + 1))

  /**
    * P25 (*) Generate a random permutation of the elements of a list.
    *
    * @param list
    * @tparam T
    * @return
    */
  def randomPermute[T](list: List[T]): List[T] = randomSelect(list.size, list)

  /**
    * P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
    *
    * @param n
    * @param list
    * @tparam T
    * @return
    */
  def combinations[T](n: Int, list: List[T]): List[List[T]] = {
    def com(cur: List[T], res: List[T]): List[List[T]] = {
      if (cur.size == n - 1) (for (r <- res) yield r :: cur).map(l => com(l, Nil)).flatten
      else if (cur.size == n) cur :: Nil
      else res.init.map(l => com(l :: cur, res.drop(res.indexOf(l) + 1))).flatten
    }

    com(Nil, list)
  }

  /**
    * P27 (**) Group the elements of a set into disjoint subsets. (a) (2,3,4)
    *
    * @param list
    * @tparam T
    * @return
    */
  def group3[T](list: List[T]): List[List[List[T]]] = {
    combinations(2, list).map(a=>a::Nil)
      .flatMap(a=>for(l<-combinations(3, list.filterNot(a.flatten.toSet))) yield l::a)
      .flatMap(a=>for(l<-combinations(4, list.filterNot(a.flatten.toSet))) yield l::a)
  }

  /**
    * P27 (**) Group the elements of a set into disjoint subsets. (b)
    *
    * @param sizes
    * @param list
    * @tparam T
    * @return
    */
  def group[T](sizes:List[Int], list: List[T]): List[List[List[T]]]={
    def gru(sizesg:List[Int], listg: List[List[List[T]]]): List[List[List[T]]] = sizesg match {
      case Nil => listg
      case head::tail => gru(tail, listg.flatMap(a=>for(l<-combinations(head, list.filterNot(a.flatten.toSet))) yield l::a))
    }
    gru(sizes.tail, combinations(sizes.head, list).map(_::Nil))
  }

  /**
    * P28 (**) Sorting a list of lists according to length of sublists. (a)
    *
    * @param list
    * @tparam T
    * @return
    */
  def lsort[T](list: List[List[T]]):List[List[T]]=list.sortWith(_.size<_.size)

  def lsortFreq[T](list: List[List[T]]):List[List[T]]={
    def freq(size: Int):Int=list.filter(_.size==size).size
    list.sortWith((a,b)=>freq(a.size)<freq(b.size))
  }

}