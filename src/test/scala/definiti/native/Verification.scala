package definiti.native

sealed trait Verification[A] {
  final def verify[B <: A](value: B): Validation[B] = validate("", value)

  private[native] def validate[B <: A](path: String, value: B): Validation[B]

  def andThen(definedVerification: DefinedVerification[A]): Verification[A] = {
    new VerificationGroup[A](Seq(this, new ValueVerification[A](definedVerification)))
  }

  def andThen(verification: Verification[A]): Verification[A] = {
    new VerificationGroup[A](Seq(this, verification))
  }

  def andBefore(beforeVerification: Verification[A]): Verification[A] = {
    new VerificationGroup[A](Seq(beforeVerification, this))
  }

  def from[B](f: B => A, path: String): Verification[B] = {
    new VerificationMap[A, B](this, f, path)
  }
}

object Verification {
  def apply[A](definedVerification: DefinedVerification[A]): Verification[A] = {
    new ValueVerification(definedVerification)
  }

  def none[A]: Verification[A] = new NoVerification[A]

  def all[A](verifications: Verification[A]*)(implicit dummyImplicit: DummyImplicit): Verification[A] = {
    all(verifications)
  }
  def all[A](verifications: Seq[Verification[A]]): Verification[A] = {
    new VerificationGroup(verifications)
  }

  implicit def definedVerificationToVerification[A](definedVerification: DefinedVerification[A]): Verification[A] = {
    new ValueVerification[A](definedVerification)
  }
}

trait DefinedVerification[A] {
  def verify(value: A): Option[Message]
}

abstract class SimpleVerification[A](message: String) extends DefinedVerification[A] {
  override def verify(value: A): Option[Message] = {
    if (isValid(value)) {
      None
    } else {
      Some(Message(message))
    }
  }

  def isValid(value: A): Boolean
}

final class VerificationGroup[A](verifications: Seq[Verification[A]]) extends Verification[A] {
  override private[native] def validate[B <: A](path: String, value: B) = {
    val validations = verifications.map(_.validate(path, value))
    if (validations.forall(_.isValid)) {
      Valid(value)
    } else {
      Validation.squashErrors(validations)
    }
  }
}

final class VerificationMap[A, C](verification: Verification[A], map: C => A, subPath: String) extends Verification[C] {
  override private[native] def validate[B <: C](path: String, value: B): Validation[B] = {
    val innerPath = if (path.nonEmpty) path + "." + subPath else subPath
    verification.validate(innerPath, map(value)) match {
      case Valid(_) => Valid(value)
      case Invalid(errors) => Invalid(errors)
    }
  }
}

final class NoVerification[A] extends Verification[A] {
  override private[native] def validate[B <: A](path: String, value: B): Validation[B] = Valid(value)
}

final class ValueVerification[A](definedVerification: DefinedVerification[A]) extends Verification[A] {
  override private[native] def validate[B <: A](path: String, value: B): Validation[B] = {
    definedVerification.verify(value) match {
      case Some(resultingMessage) => Invalid(Error(path, resultingMessage))
      case None => Valid(value)
    }
  }
}

final class ListVerification[A](verification: Verification[A] = Verification.none[A]) extends Verification[List[A]] {
  override private[native] def validate[B <: List[A]](path: String, value: B) = {
    val validations = value.zipWithIndex.map {
      case (current, index) => verification.validate(path + s"[${index}]", current)
    }
    if (validations.forall(_.isValid)) {
      Valid(value)
    } else {
      Validation.squashErrors(validations)
    }
  }
}

final class OptionVerification[A](verification: Verification[A] = Verification.none[A]) extends Verification[Option[A]] {
  override private[native] def validate[B <: Option[A]](path: String, value: B) = {
    value
      .map(verification.validate(path, _))
      .map {
        case Valid(_) => Valid(value)
        case Invalid(errors) => Invalid(errors)
      }
      .getOrElse(Valid(value))
  }
}