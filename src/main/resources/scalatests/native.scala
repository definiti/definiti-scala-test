package definiti.scalatests

import java.sql.Date

import org.scalacheck.Gen

package object native {
  def anyBoolean(): Gen[Boolean] = ???

  def anyDate(): Gen[Date] = ???

  def passedDate(): Gen[Date] = ???

  def futureDate(): Gen[Date] = ???

  def list[A](elements: A*): Gen[List[A]] = ???

  def listOf[A](elements: A): Gen[List[A]] = ???

  def nonEmptyListOf[A](elements: A): Gen[List[A]] = ???

  def boundedListOf[A](min: Number, max: Number, elements: A): Gen[List[A]] = ???

  def oneOf[A](elements: A*): Gen[A] = ???

  def anyNumber(): Gen[Number] = ???

  def positiveNumber(): Gen[Number] = ???

  def negativeNumber(): Gen[Number] = ???

  def numberBetween(min: Number, max: Number): Gen[Number] = ???

  def optionOf[A](element: A): Gen[Option[A]] = ???

  def some[A](element: A): Gen[Option[A]] = ???

  def none[A](): Gen[Option[A]] = ???

  def anyChar(): Gen[String] = ???

  def anyString(): Gen[String] = ???

  def alphaChar(): Gen[String] = ???

  def alphaString(): Gen[String] = ???

  def numericChar(): Gen[String] = ???

  def numericString(): Gen[String] = ???

  def alphaNumericString(): Gen[String] = ???

  def nonEmptyString(): Gen[String] = ???

  def nonEmptyAlphaString(): Gen[String] = ???

  def nonEmptyNumericString(): Gen[String] = ???

  def nonEmptyAlphaNumericString(): Gen[String] = ???

  def stringOf(min: Number, max: Number): Gen[String] = ???

  def alphaStringOf(min: Number, max: Number): Gen[String] = ???

  def numericStringOf(min: Number, max: Number): Gen[String] = ???

  def alphaNumericStringOf(min: Number, max: Number): Gen[String] = ???
}
