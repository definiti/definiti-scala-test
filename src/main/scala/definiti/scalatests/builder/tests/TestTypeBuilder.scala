package definiti.scalatests.builder.tests

import definiti.common.ast._
import definiti.scalatests.builder.common.GenExpressionBuilder
import definiti.scalatests.{ast => scalaAst}
import definiti.tests.ast.GeneratorMeta
import definiti.tests.validation.helpers.ScopedExpression
import definiti.tests.{ast => testsAst}

object TestTypeBuilder {
  def buildTestType(testType: testsAst.TestType, generators: Seq[GeneratorMeta])(implicit library: Library, coreGenerators: Seq[GeneratorMeta]): Seq[scalaAst.Statement] = {
    val typ = library.typesMap(testType.typ.name)
    val testCases = extractTestCases(testType)
    buildTestTypes(typ, testCases, generators)
  }

  private def extractTestCases(testType: testsAst.TestType): Seq[TestCase] = {
    testType.cases.flatMap { testCase =>
      testCase.subCases.map { subCase =>
        TestCase(testCase, subCase)
      }
    }
  }

  private def buildTestTypes(typ: ClassDefinition, testCases: Seq[TestCase], generators: Seq[GeneratorMeta])(implicit library: Library, coreGenerators: Seq[GeneratorMeta]): Seq[scalaAst.Statement] = {
    testCases.zipWithIndex.map { case (testCase, index) =>
      scalaAst.TestDeclaration(
        subject = s"Type ${typ.fullName}",
        name = testCase.testCase.comment match {
          case Some(comment) => s"${comment} (case ${index})"
          case None => s"case ${index}"
        },
        body = buildTestTypeBody(typ, testCase, generators)
      )
    }
  }

  private def buildTestTypeBody(typ: ClassDefinition, testCase: TestCase, generators: Seq[GeneratorMeta])(implicit library: Library, coreGenerators: Seq[GeneratorMeta]): scalaAst.Expression = {
    val scopedExpression = new ScopedExpression[testsAst.Expression](testCase.subCase.expression, Map.empty, generators, library)
    scalaAst.CallHigherOrderFunction(
      target = scalaAst.Value("forAll"),
      arguments = Seq(GenExpressionBuilder.buildGenExpression(scopedExpression)),
      functionArguments = Seq("input"),
      functionBody = buildTestTypeBodyAssertion(buildTypeVerificationCall(typ, testCase), typ, testCase),
      generics = Seq.empty
    )
  }

  private def buildTypeVerificationCall(typ: ClassDefinition, testCase: TestCase): scalaAst.Expression = {
    scalaAst.CallMethod(
      target = scalaAst.CallAttribute(
        target = scalaAst.Value(typ.fullName),
        name = "verification"
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

  case class TestCase(
    testCase: testsAst.Case,
    subCase: testsAst.SubCase
  )

}