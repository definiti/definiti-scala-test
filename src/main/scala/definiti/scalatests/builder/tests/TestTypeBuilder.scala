package definiti.scalatests.builder.tests

import definiti.common.ast._
import definiti.scalatests.builder.BuilderContext
import definiti.scalatests.builder.common.{GenExpressionBuilder, TypeBuilder}
import definiti.scalatests.{ast => scalaAst}
import definiti.tests.validation.helpers.ScopedExpression
import definiti.tests.{ast => testsAst}

object TestTypeBuilder {
  def buildTestType(typ: testsAst.Type, testCases: Seq[TestCase])(implicit builderContext: BuilderContext): Seq[scalaAst.Statement] = {
    val classDefinition = builderContext.library.typesMap(typ.name)
    buildTestTypes(classDefinition, typ, testCases)
  }

  private def buildTestTypes(classDefinition: ClassDefinition, typ: testsAst.Type, testCases: Seq[TestCase])(implicit builderContext: BuilderContext): Seq[scalaAst.Statement] = {
    testCases.zipWithIndex.map { case (testCase, index) =>
      scalaAst.TestDeclaration(
        subject = s"Type ${classDefinition.fullName}",
        name = testCase.testCase.comment match {
          case Some(comment) => s"${comment} (case ${index})"
          case None => s"case ${index}"
        },
        body = buildTestTypeBody(classDefinition, typ, testCase)
      )
    }
  }

  private def buildTestTypeBody(classDefinition: ClassDefinition, typ: testsAst.Type, testCase: TestCase)(implicit builderContext: BuilderContext): scalaAst.Expression = {
    val scopedExpression = new ScopedExpression[testsAst.Expression](testCase.subCase.expression, Map.empty, Seq.empty, builderContext.generators, builderContext.library)
    scalaAst.CallHigherOrderFunction(
      target = scalaAst.Value("forAll"),
      arguments = Seq(GenExpressionBuilder.buildGenExpression(scopedExpression)),
      functionArguments = Seq("input"),
      functionBody = buildTestTypeBodyAssertion(buildTypeVerificationCall(classDefinition, typ, testCase), classDefinition, testCase),
      generics = Seq.empty
    )
  }

  private def buildTypeVerificationCall(classDefinition: ClassDefinition, typ: testsAst.Type, testCase: TestCase): scalaAst.Expression = {
    scalaAst.CallMethod(
      target = scalaAst.CallMethod(
        target = scalaAst.Value(classDefinition.fullName),
        name = "verification",
        arguments = Seq.empty,
        generics = typ.generics.map(TypeBuilder.buildType)
      ),
      name = "verify",
      arguments = Seq(scalaAst.Value("input"))
    )
  }

  private def buildTestTypeBodyAssertion(result: scalaAst.Expression, typ: ClassDefinition, testCase: TestCase): scalaAst.Expression = {
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
          name = "shouldBe",
          arguments = Seq(scalaAst.Value("an[Invalid]"))
        )
    }
  }

}