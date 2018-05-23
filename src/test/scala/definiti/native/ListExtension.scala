package definiti.native

object ListExtension {
  @inline def isEmpty[A](list: Seq[A]): Boolean = list.isEmpty

  @inline def nonEmpty[A](list: Seq[A]): Boolean = list.nonEmpty

  @inline def head[A](list: Seq[A]): Option[A] = list.headOption

  @inline def tail[A](list: Seq[A]): Seq[A] = list.tail

  @inline def last[A](list: Seq[A]): Option[A] = list.lastOption

  @inline def map[A, B](list: Seq[A], mapping: A => B): Seq[B] = list.map(mapping)

  @inline def flatMap[A, B](list: Seq[A], mapping: A => Seq[B]): Seq[B] = list.flatMap(mapping)

  @inline def forAll[A](list: Seq[A], predicate: A => Boolean): Boolean = list.forall(predicate)

  @inline def forNone[A](list: Seq[A], predicate: A => Boolean): Boolean = !list.exists(predicate)

  @inline def filter[A](list: Seq[A], predicate: A => Boolean): Seq[A] = list.filter(predicate)

  @inline def filterNot[A](list: Seq[A], predicate: A => Boolean): Seq[A] = list.filterNot(predicate)

  @inline def exists[A](list: Seq[A], predicate: A => Boolean): Boolean = list.exists(predicate)

  @inline def existsNot[A](list: Seq[A], predicate: A => Boolean): Boolean = !list.exists(predicate)

  @inline def contains[A](list: Seq[A], element: A): Boolean = list.contains(element)

  @inline def containsNot[A](list: Seq[A], element: A): Boolean = !list.contains(element)

  @inline def foldLeft[A, B](list: Seq[A], initialValue: B, operation: (B, A) => B): B = list.foldLeft(initialValue)(operation)

  @inline def foldRight[A, B](list: Seq[A], initialValue: B, operation: (A, B) => B): B = list.foldRight(initialValue)(operation)

  @inline def indexOf[A](list: Seq[A], element: A): BigDecimal = list.indexOf(element)

  @inline def mkString[A](list: Seq[A], prefix: String, separator: String, suffix: String): String = list.mkString(prefix, separator, suffix)

  @inline def size[A](list: Seq[A]): BigDecimal = list.size

  @inline def distinct[A](list: Seq[A]): Seq[A] = list.distinct

  @inline def intersect[A](list: Seq[A], other: Seq[A]): Seq[A] = list.intersect(other)

  @inline def union[A](list: Seq[A], other: Seq[A]): Seq[A] = list.union(other)

  @inline def count[A](list: Seq[A], predicate: A => Boolean): BigDecimal = list.count(predicate)

  @inline def reverse[A](list: Seq[A]): Seq[A] = list.reverse

  @inline def find[A](list: Seq[A], predicate: A => Boolean): Option[A] = list.find(predicate)

  @inline def findLast[A](list: Seq[A], predicate: A => Boolean): Option[A] = list.reverse.find(predicate)

  @inline def drop[A](list: Seq[A], numberOfElementsToDrop: BigDecimal): Seq[A] = list.drop(numberOfElementsToDrop.toInt)

  @inline def dropWhile[A](list: Seq[A], predicate: A => Boolean): Seq[A] = list.dropWhile(predicate)

  @inline def take[A](list: Seq[A], numberOfElementsToTake: BigDecimal): Seq[A] = list.take(numberOfElementsToTake.toInt)

  @inline def takeWhile[A](list: Seq[A], predicate: A => Boolean): Seq[A] = list.takeWhile(predicate)

  @inline def append[A](list: Seq[A], element: A): Seq[A] = list :+ element

  @inline def prepend[A](list: Seq[A], element: A): Seq[A] = element +: list

  @inline def appendAll[A](list: Seq[A], elements: Seq[A]): Seq[A] = list ++ elements

  @inline def prependAll[A](list: Seq[A], elements: Seq[A]): Seq[A] = elements ++ list
}
