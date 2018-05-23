package definiti.native

object OptionExtension {
  @inline def isEmpty[A](option: Option[A]): Boolean = option.isEmpty

  @inline def nonEmpty[A](option: Option[A]): Boolean = option.nonEmpty

  @inline def getOrElse[A](option: Option[A], other: => A): A = option.getOrElse(other)

  @inline def orElse[A](option: Option[A], other: => Option[A]): Option[A] = option.orElse(other)

  @inline def map[A, B](option: Option[A], mapping: A => B): Option[B] = option.map(mapping)

  @inline def flatMap[A, B](option: Option[A], mapping: A => Option[B]): Option[B] = option.flatMap(mapping)

  @inline def filter[A](option: Option[A], predicate: A => Boolean): Option[A] = option.filter(predicate)

  @inline def filterNot[A](option: Option[A], predicate: A => Boolean): Option[A] = option.filterNot(predicate)

  @inline def exists[A](option: Option[A], predicate: A => Boolean): Boolean = option.exists(predicate)

  @inline def existsNot[A](option: Option[A], predicate: A => Boolean): Boolean = option.exists(!predicate(_))

  @inline def contains[A](option: Option[A], element: A): Boolean = option.contains(element)

  @inline def containsNot[A](option: Option[A], element: A): Boolean = !option.contains(element)

  def containsIfDefined[A](option: Option[A], elementOption: A): Boolean = elementOption match {
    case Some(element) => option.contains(element)
    case None => true
  }
}