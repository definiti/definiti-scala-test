package definiti.scalatests.builder.tests

import definiti.common.ast.{LiteralMessage, TypedMessage, Verification}
import definiti.scalatests.builder.BuilderContext
import definiti.scalatests.builder.common.{ExpressionBuilder, GenExpressionBuilder}
import definiti.scalatests.{ast => scalaAst}
import definiti.tests.ast.GeneratorMeta
import definiti.tests.{ast => testsAst}

object TestVerificationBuilder {
  def buildTestVerification(verificationName: String, testCases: Seq[TestCase])(implicit builderContext: BuilderContext): Seq[scalaAst.Statement] = {
    val verification = builderContext.library.verificationsMap(verificationName)
    buildTestVerifications(verification, testCases)
  }

  private def buildTestVerifications(verification: Verification, testCases: Seq[TestCase])(implicit builderContext: BuilderContext): Seq[scalaAst.Statement] = {
    testCases.zipWithIndex.map { case (testCase, index) =>
      scalaAst.TestDeclaration(
        subject = if (index == 0) s"Verification ${verification.fullName}" else "it",
        name = testCase.testCase.comment match {
          case Some(comment) => s"${comment} (case ${index})"
          case None =>
            val kind = testCase.testCase.kind match {
              case testsAst.CaseKind.accept => "be valid"
              case testsAst.CaseKind.refuse => "be invalid"
            }
            s"${kind} for case ${index + 1}"
        },
        body = buildTestVerificationBody(verification, testCase)
      )
    }
  }

  private def buildTestVerificationBody(verification: Verification, testCase: TestCase)(implicit builderContext: BuilderContext): scalaAst.Expression = {
    scalaAst.CallHigherOrderFunction(
      target = scalaAst.Value("forAll"),
      arguments = Seq(GenExpressionBuilder.buildGenExpression(ExpressionBuilder.scopedExpression(testCase.subCase.expression))),
      functionArguments = Seq("input"),
      functionBody = buildTestVerificationBodyAssertion(buildVerificationCall(verification, testCase), verification, testCase),
      generics = Seq.empty
    )
  }

  private def buildVerificationCall(verification: Verification, testCase: TestCase)(implicit builderContext: BuilderContext): scalaAst.Expression = {
    scalaAst.CallMethod(
      target = scalaAst.New(
        clazz = verification.fullName,
        arguments = testCase.subCase.arguments.map(ExpressionBuilder.scopedExpression).map(ExpressionBuilder.buildSimpleExpression),
        generics = Seq.empty
      ),
      name = "verify",
      arguments = Seq(scalaAst.Value("input"))
    )

  }

  private def buildTestVerificationBodyAssertion(result: scalaAst.Expression, verification: Verification, testCase: TestCase)(implicit builderContext: BuilderContext): scalaAst.Expression = {
    testCase.testCase.kind match {
      case testsAst.CaseKind.accept =>
        scalaAst.CallMethod(
          target = result,
          name = "should",
          arguments = Seq(function("===", scalaAst.Value("None")))
        )
      case testsAst.CaseKind.refuse =>
        if (testCase.subCase.messageArguments.nonEmpty) {
          scalaAst.CallMethod(
            target = result,
            name = "should",
            arguments = Seq(
              function("===", function("Some",
                buildVerificationMessage(verification, testCase)
              ))
            )
          )
        } else {
          scalaAst.CallMethod(
            target = result,
            name = "shouldBe",
            arguments = Seq(scalaAst.Value("a[Some[_]]"))
          )
        }
    }
  }

  private def function(target: String, arguments: scalaAst.Expression*): scalaAst.Expression = {
    scalaAst.CallFunction(
      target = scalaAst.Value(target),
      arguments = arguments
    )
  }

  private def buildVerificationMessage(verification: Verification, testCase: TestCase)(implicit builderContext: BuilderContext): scalaAst.Expression = {
    verification.message match {
      case literal: LiteralMessage => function("Message0", scalaAst.StringExpression(literal.message))
      case typed: TypedMessage =>
        val args = testCase.subCase.messageArguments
        function(s"Message${args.length}", scalaAst.StringExpression(typed.message) +: args.map(ExpressionBuilder.scopedExpression).map(ExpressionBuilder.buildSimpleExpression): _*)
    }
  }
}
