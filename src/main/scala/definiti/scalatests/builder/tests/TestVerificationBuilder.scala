package definiti.scalatests.builder.tests

import definiti.common.ast.{Library, LiteralMessage, TypedMessage, Verification}
import definiti.scalatests.builder.common.{ExpressionBuilder, GenExpressionBuilder}
import definiti.scalatests.{ast => scalaAst}
import definiti.tests.ast.GeneratorMeta
import definiti.tests.{ast => testsAst}

object TestVerificationBuilder {
  def buildTestVerification(testVerification: testsAst.TestVerification, generators: Seq[GeneratorMeta])(implicit library: Library, coreGenerators: Seq[GeneratorMeta]): Seq[scalaAst.Statement] = {
    val verification = library.verificationsMap(testVerification.verification)
    val testCases = extractTestCases(testVerification)
    buildTestVerifications(verification, testCases, generators)
  }

  private def extractTestCases(testVerification: testsAst.TestVerification): Seq[TestCase] = {
    testVerification.cases.flatMap { testCase =>
      testCase.subCases.map { subCase =>
        TestCase(testCase, subCase)
      }
    }
  }

  private def buildTestVerifications(verification: Verification, testCases: Seq[TestCase], generators: Seq[GeneratorMeta])(implicit library: Library, coreGenerators: Seq[GeneratorMeta]): Seq[scalaAst.Statement] = {
    testCases.zipWithIndex.map { case (testCase, index) =>
      scalaAst.TestDeclaration(
        subject = s"Verification ${verification.fullName}",
        name = testCase.testCase.comment match {
          case Some(comment) => s"${comment} (case ${index})"
          case None => s"case ${index}"
        },
        body = buildTestVerificationBody(verification, testCase, generators)
      )
    }
  }

  private def buildTestVerificationBody(verification: Verification, testCase: TestCase, generators: Seq[GeneratorMeta])(implicit library: Library, coreGenerators: Seq[GeneratorMeta]): scalaAst.Expression = {
    scalaAst.CallHigherOrderFunction(
      target = scalaAst.Value("forAll"),
      arguments = Seq(GenExpressionBuilder.buildGenExpression(ExpressionBuilder.scopedExpression(testCase.subCase.expression, generators))),
      functionArguments = Seq("input"),
      functionBody = buildTestVerificationBodyAssertion(buildVerificationCall(verification, testCase, generators), verification, testCase, generators),
      generics = Seq.empty
    )
  }

  private def buildVerificationCall(verification: Verification, testCase: TestCase, generators: Seq[GeneratorMeta])(implicit library: Library): scalaAst.Expression = {
    scalaAst.CallMethod(
      target = scalaAst.New(
        clazz = verification.fullName,
        arguments = testCase.subCase.arguments.map(ExpressionBuilder.scopedExpression(_, generators)).map(ExpressionBuilder.buildSimpleExpression),
        generics = Seq.empty
      ),
      name = "verify",
      arguments = Seq(scalaAst.Value("input"))
    )

  }

  private def buildTestVerificationBodyAssertion(result: scalaAst.Expression, verification: Verification, testCase: TestCase, generators: Seq[GeneratorMeta])(implicit library: Library): scalaAst.Expression = {
    testCase.testCase.kind match {
      case testsAst.CaseKind.accept =>
        scalaAst.CallMethod(
          target = result,
          name = "shouldBe",
          arguments = Seq(scalaAst.Value("a[Valid[_]]"))
        )
      case testsAst.CaseKind.refuse =>
        scalaAst.CallMethod(
          target = result,
          name = "should",
          arguments = Seq(
            function("===", function("Invalid", function("Seq",
              function("Error",
                scalaAst.StringExpression(""),
                function("Seq", buildVerificationMessage(verification, testCase, generators))
              )
            )))
          )
        )
    }
  }

  private def function(target: String, arguments: scalaAst.Expression*): scalaAst.Expression = {
    scalaAst.CallFunction(
      target = scalaAst.Value(target),
      arguments = arguments
    )
  }

  private def buildVerificationMessage(verification: Verification, testCase: TestCase, generators: Seq[GeneratorMeta])(implicit library: Library): scalaAst.Expression = {
    verification.message match {
      case literal: LiteralMessage => function("Message0", scalaAst.StringExpression(literal.message))
      case _: TypedMessage =>
        val args = testCase.subCase.messageArguments
        function(s"Message${args.length}", args.map(ExpressionBuilder.scopedExpression(_, generators)).map(ExpressionBuilder.buildSimpleExpression): _*)
    }
  }

  case class TestCase(
    testCase: testsAst.Case,
    subCase: testsAst.SubCase
  )

}
