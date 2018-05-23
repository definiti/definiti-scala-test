package definiti.native

sealed trait Validation[+A] {
  def isValid: Boolean

  def map[B](f: A => B): Validation[B]

  def flatMap[B](f: A => Validation[B]): Validation[B]

  def andThen[B](f: => Validation[B]): Validation[B]
}

object Validation {
  def squashErrors(validations: Seq[Validation[_]]): Invalid = {
    Invalid {
      validations
        .collect { case Invalid(errors) => errors }
        .flatten
        .groupBy(_.path)
        .map { case (errorPath, errors) => Error(errorPath, errors.flatMap(_.messages)) }
        .toSeq
    }
  }
}

case class Valid[+A](value: A) extends Validation[A] {
  override def isValid: Boolean = true

  override def map[B](f: A => B): Validation[B] = Valid(f(value))

  override def flatMap[B](f: A => Validation[B]): Validation[B] = f(value)

  override def andThen[B](f: => Validation[B]): Validation[B] = f
}

case class Invalid(errors: Seq[Error]) extends Validation[Nothing] {
  override def isValid: Boolean = false

  override def map[B](f: (Nothing) => B): Validation[B] = Invalid(errors)

  override def flatMap[B](f: Nothing => Validation[B]): Validation[Nothing] = Invalid(errors)

  override def andThen[B](f: => Validation[B]): Validation[B] = Invalid(errors)
}

object Invalid {
  def apply(errors: Error*)(implicit dummyImplicit: DummyImplicit): Invalid = new Invalid(errors)

  def apply(path: String, messages: Message*): Invalid = Invalid(Error(path, messages))

  def root(messages: Seq[Message]): Invalid = Invalid(Error("", messages))

  def root(messages: Message*)(implicit dummyImplicit: DummyImplicit): Invalid = root(messages)
}

case class Error(path: String, messages: Seq[Message])

object Error {
  def apply(path: String, messages: Message*)(implicit dummyImplicit: DummyImplicit): Error = new Error(path, messages)
}