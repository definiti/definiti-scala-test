package definiti.scalatests

import java.time.{LocalDateTime, ZoneId, ZoneOffset}

import org.scalacheck.{Arbitrary, Gen}

package object native {
  def initializeGenContext: Gen[Unit] = Gen.const()

  def anyBoolean(): Gen[Boolean] = Arbitrary.arbBool.arbitrary

  def anyDate(): Gen[LocalDateTime] = Arbitrary.arbDate.arbitrary.map(_.toInstant).map(LocalDateTime.ofInstant(_, ZoneId.systemDefault))

  def passedDate(): Gen[LocalDateTime] = for {
    date <- anyDate()
    current = LocalDateTime.now(ZoneId.systemDefault())
  } yield {
    if (date.isBefore(current)) {
      date
    } else {
      val currentEpochSecond = current.toEpochSecond(ZoneOffset.UTC)
      val difference = date.toEpochSecond(ZoneOffset.UTC) - currentEpochSecond
      LocalDateTime.ofEpochSecond(currentEpochSecond - difference, 0, ZoneOffset.UTC)
    }
  }

  def futureDate(): Gen[LocalDateTime] = for {
    date <- anyDate()
    current = LocalDateTime.now(ZoneId.systemDefault())
  } yield {
    if (date.isAfter(current)) {
      date
    } else {
      val currentEpochSecond = current.toEpochSecond(ZoneOffset.UTC)
      val difference = currentEpochSecond - date.toEpochSecond(ZoneOffset.UTC)
      LocalDateTime.ofEpochSecond(currentEpochSecond + difference, 0, ZoneOffset.UTC)
    }
  }

  def list[A](elements: A*): Gen[List[A]] = Gen.const(List(elements: _*))

  def listOf[A](elements: Gen[A]): Gen[List[A]] = Gen.listOf(elements)

  def nonEmptyListOf[A](elements: Gen[A]): Gen[List[A]] = Gen.nonEmptyListOf(elements)

  def boundedListOf[A](min: BigDecimal, max: BigDecimal, elements: Gen[A]): Gen[List[A]] = for {
    n <- Gen.choose(Math.min(min.toInt, max.toInt), Math.max(min.toInt, max.toInt))
    result <- Gen.listOfN(n, elements)
  } yield result

  def takeOneOf[A](elements: Gen[A]*): Gen[A] = {
    if (elements.isEmpty) {
      throw new IllegalArgumentException("oneOf called on empty collection")
    } else if (elements.length == 1) {
      elements.head
    } else {
      Gen.oneOf(elements(0), elements(1), elements.drop(2): _*)
    }
  }

  def anyNumber(): Gen[BigDecimal] = Arbitrary.arbBigDecimal.arbitrary

  def positiveNumber(): Gen[BigDecimal] = Gen.posNum[Double].map(BigDecimal(_))

  def negativeNumber(): Gen[BigDecimal] = Gen.negNum[Double].map(BigDecimal(_))

  def numberBetween(min: BigDecimal, max: BigDecimal): Gen[BigDecimal] = Gen.choose(min.toDouble, max.toDouble).map(BigDecimal(_))

  def optionOf[A](element: Gen[A]): Gen[Option[A]] = Gen.option(element)

  def some[A](element: A): Gen[Option[A]] = Gen.some(Gen.const(element))

  def someOf[A](element: Gen[A]): Gen[Option[A]] = Gen.some(element)

  def none[A](): Gen[Option[A]] = Gen.const(None)

  def anyChar(): Gen[String] = Arbitrary.arbChar.arbitrary.map(_.toString)

  def anyString(): Gen[String] = Arbitrary.arbString.arbitrary

  def alphaChar(): Gen[String] = Gen.alphaChar.map(_.toString)

  def alphaString(): Gen[String] = Gen.alphaStr

  def numericChar(): Gen[String] = Gen.numChar.map(_.toString)

  def numericString(): Gen[String] = Gen.numStr

  def alphaNumericString(): Gen[String] = Gen.alphaNumStr

  def nonEmptyString(): Gen[String] = for {
    firstChar <- anyChar()
    rest <- anyString()
  } yield firstChar + rest

  def nonEmptyAlphaString(): Gen[String] = for {
    firstChar <- alphaChar()
    rest <- alphaString()
  } yield firstChar + rest

  def nonEmptyNumericString(): Gen[String] = for {
    firstChar <- numericChar()
    rest <- numericString()
  } yield firstChar + rest

  def nonEmptyAlphaNumericString(): Gen[String] = for {
    firstChar <- Gen.oneOf(alphaChar(), numericChar())
    rest <- alphaNumericString()
  } yield firstChar + rest

  def stringOf(min: BigDecimal, max: BigDecimal): Gen[String] = for {
    n <- Gen.choose(Math.min(min.toInt, max.toInt), Math.max(min.toInt, max.toInt))
    result <- Gen.listOfN(n, anyChar())
  } yield result.mkString

  def alphaStringOf(min: BigDecimal, max: BigDecimal): Gen[String] = for {
    n <- Gen.choose(Math.min(min.toInt, max.toInt), Math.max(min.toInt, max.toInt))
    result <- Gen.listOfN(n, alphaChar())
  } yield result.mkString

  def numericStringOf(min: BigDecimal, max: BigDecimal): Gen[String] = for {
    n <- Gen.choose(Math.min(min.toInt, max.toInt), Math.max(min.toInt, max.toInt))
    result <- Gen.listOfN(n, numericChar())
  } yield result.mkString

  def alphaNumericStringOf(min: BigDecimal, max: BigDecimal): Gen[String] = for {
    n <- Gen.choose(Math.min(min.toInt, max.toInt), Math.max(min.toInt, max.toInt))
    result <- Gen.listOfN(n, Gen.oneOf(alphaChar(), numericChar()))
  } yield result.mkString
}

