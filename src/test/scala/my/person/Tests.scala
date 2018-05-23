package my.person

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import definiti.scalatests.native._
import definiti.native._

class Tests extends FlatSpec with Matchers with PropertyChecks {
  /*
  type Person {
  firstName: String verifying IsNonBlank
  lastName: String verifying IsNonBlank
  phone: String verifying IsLengthOf(10, 12)

  verify {
    "First name and last name should not be the same"
    (person) => {
      person.firstName != person.lastName
    }
  }
}
   */

  case class Person(firstName: String, lastName: String, phone: String)

  object Person {
    val verification: Verification[Person] = Verification.none[Person]
  }

  // generator name(): String = alphaChar().toUpperCase().append(alphaString().toLowerCase())
  def name(): Gen[String] = for {
    v1 <- alphaChar()
    v2 = StringExtension.toUpperCase(v1)
    v3 <- alphaString()
    v4 = StringExtension.toLowerCase(v3)
    v5 = StringExtension.append(v2, v4)
  } yield v5

  // alphaChar().toUpperCase().append(alphaString().toLowerCase())
  // append(toUpperCase(alphaChar()), toLowerCase(alphaString()))
  def name2(): Gen[String] = for {
    target <- {
      for {
        target <- alphaChar()
      } yield {
        StringExtension.toUpperCase(target)
      }
    }
    arg1 <- {
      for {
        target <- alphaString()
      } yield {
        StringExtension.toLowerCase(target)
      }
    }
  } yield {
    StringExtension.append(target, arg1)
  }

  def const(): Gen[String] = Gen.const("")

  // generator phoneString(): String = numericStringOf(10, 10)
  def phoneString(): Gen[String] = numericStringOf(10, 10)

  /*
  generator validPerson(): Person = Person {
    firstName: name()
    lastName: name()
    phone: phoneString()
  }
   */
  def validPerson(): Gen[Person] = for {
    firstName <- name()
    lastName <- name()
    phone <- phoneString()
  } yield {
    Person(
      firstName = firstName,
      lastName = lastName,
      phone = phone
    )
  }

  "Type Person" should "Accept a person with non empty values" in {
    forAll(for {
      firstName <- nonEmptyString()
      lastName <- nonEmptyString()
      phone <- phoneString()
    } yield {
      Person(
        firstName = firstName,
        lastName = lastName,
        phone = phone
      )
    }) { person =>
      Person.verification.verify(person).isValid should ===(true)
    }
  }

  "Type Person" should "accept case 2" in {
    forAll(validPerson()) { person =>
      Person.verification.verify(person).isValid.should(===(true))
    }
  }

  /*
  test verification IsNonBlank {
    accept "a"
    refuse ""
  }
   */

  class IsNonBlank(message: String = "The string is blank /* quoted comment */") extends SimpleVerification[String](message) {
    override def isValid(string: String): Boolean = string.trim.nonEmpty
  }

  "Verification IsNonBlank" should "accept case 1" in {
    forAll(Gen.const("a")) { input =>
      val result = new IsNonBlank().verify(input)
      result.shouldBe(a[Valid[_]])
    }
  }

  it should "refuse case 2" in {
    val result = new IsNonBlank().verify("")
    result.should(===(Invalid(Seq(
      Error("", Seq(Message0("The string is blank /* quoted comment */")))
    ))))
  }

  /*
  test verification IsLengthOf {
    accept
      "" with (0, 1)
      "a" with (0, 1)
      "ab" with (0, 2)
      "ab" with (1, 2)
      "abcde" with (1, 10)

    refuse
      "" with (1, 2) as (1, 2)
      "a" with (2, 3) as (2, 3)
      "abcde" with (6, 10) as (6, 10)
      "abcdefghijk" with (6, 10) as (6, 10)
  }
   */

  class StringOfLength(min: BigDecimal, max: BigDecimal, message: String = "string.of.length") extends DefinedVerification[String] {
    override def verify(string: String): Option[Message] = {
      if ((min <= StringExtension.length(string)) && (StringExtension.length(string) <= max)) None
      else Some(Message(message, min, max))
    }
  }

  "Verification IsLengthOf" should "accept case 1" in {
    val result = new StringOfLength(0, 1).verify("")
    result.shouldBe(a[Valid[_]])
  }

  it should "accept case 2" in {
    val result = new StringOfLength(0, 1).verify("a")
    result.shouldBe(a[Valid[_]])
  }

  it should "refuse case n" in {
    val result = new StringOfLength(1, 2).verify("")
    result.should(===(Invalid(Seq(
      Error("", Seq(Message2("string.of.length", 1, 2)))
    ))))
  }

  it should "refuse case m" in {
    val result = new StringOfLength(2, 3).verify("a")
    result.should(===(Invalid(Seq(
      Error("", Seq(Message2("string.of.length", 2, 3)))
    ))))
  }
}
