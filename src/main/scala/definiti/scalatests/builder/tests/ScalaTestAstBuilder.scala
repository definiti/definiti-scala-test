package definiti.scalatests.builder.tests

import definiti.common.ast._
import definiti.common.utils.StringUtils
import definiti.scalatests.builder.BuilderContext
import definiti.scalatests.builder.common.GeneratorsExtractor
import definiti.scalatests.{Configuration, ast => scalaAst}
import definiti.tests.ast.GeneratorMeta
import definiti.tests.{CoreGenerators, ast => testsAst}

class ScalaTestAstBuilder(config: Configuration, library: Library) {
  implicit def lib: Library = library

  def build(root: Root): scalaAst.Root = {
    implicit val builderContext: BuilderContext = BuilderContext(
      library = library,
      coreGenerators = CoreGenerators.coreGenerators.fold(_ => Seq.empty, identity),
      projectGenerators = GeneratorsExtractor.extractGenerators(root)
    )
    scalaAst.Root(
      namespaces = root.namespaces
        .map(buildNamespace)
        .filter(_.elements.nonEmpty)
    )
  }

  private def buildNamespace(namespace: Namespace)(implicit builderContext: BuilderContext): scalaAst.Namespace = {
    val tests = extractTests(namespace)
    val testVerificationStatements = extractTestVerifications(tests).flatMap { case (verification, testCases) =>
      TestVerificationBuilder.buildTestVerification(verification, testCases)
    }
    val testTypeStatements = extractTestTypes(tests).flatMap { case (typ, testCases) =>
      TestTypeBuilder.buildTestType(typ, testCases)
    }
    val namespaceName = if (namespace.fullName.isEmpty) "root" else namespace.fullName
    scalaAst.Namespace(
      name = namespaceName,
      imports = defaultImports(),
      elements = Seq(scalaAst.ClassDef(
        name = s"${StringUtils.lastPart(namespaceName).capitalize}Spec",
        extendz = Seq("FlatSpec", "Matchers", "PropertyChecks"),
        body = (testVerificationStatements ++ testTypeStatements).toSeq
      ))
    )
  }

  private def extractTests(namespace: Namespace): Seq[testsAst.Test] = {
    namespace.elements
      .collect {
        case context: ExtendedContext[testsAst.TestsContext] if context.name == "tests" => context
      }
      .flatMap(_.content.tests)
  }

  private def extractTestVerifications(tests: Seq[testsAst.Test]): Map[String, Seq[TestCase]] = {
    var testVerifications = Map.empty[String, Seq[TestCase]]
    tests.foreach {
      case testVerification: testsAst.TestVerification =>
        val testCases = testVerifications.getOrElse(testVerification.verification, Seq.empty)
        val newTestCases = testVerification.cases.flatMap(caseToTestCases)
        val newEntry = testVerification.verification -> (testCases ++ newTestCases)
        testVerifications = testVerifications + newEntry
      case _ =>
    }
    testVerifications
  }

  private def caseToTestCases(testCase: testsAst.Case): Seq[TestCase] = {
    testCase.subCases.map { subCase =>
      TestCase(testCase, subCase)
    }
  }

  private def extractTestTypes(tests: Seq[testsAst.Test]): Map[testsAst.Type, Seq[TestCase]] = {
    var testTypes = Map.empty[testsAst.Type, Seq[TestCase]]
    tests.foreach {
      case testType: testsAst.TestType =>
        val testCases = testTypes.getOrElse(testType.typ, Seq.empty)
        val newTestCases = testType.cases.flatMap(caseToTestCases)
        val newEntry = testType.typ -> (testCases ++ newTestCases)
        testTypes = testTypes + newEntry
      case _ =>
    }
    testTypes
  }

  private def defaultImports(): Seq[scalaAst.Import] = Seq(
    scalaAst.Import("org.scalacheck.Gen"),
    scalaAst.Import("org.scalatest.prop.PropertyChecks"),
    scalaAst.Import("org.scalatest.{FlatSpec, Matchers}"),
    scalaAst.Import("java.time.LocalDateTime"),
    scalaAst.Import("definiti.native._"),
    scalaAst.Import("definiti.scalatests.native._")
  )

  def buildType(typ: testsAst.Type): scalaAst.Type = {
    scalaAst.Type(
      name = typ.name,
      generics = typ.generics.map(buildType)
    )
  }

  private def extractGenerators(root: Root): Seq[testsAst.GeneratorMeta] = {
    root.namespaces
      .flatMap(_.elements)
      .collect { case extendedContext: ExtendedContext[testsAst.TestsContext] if extendedContext.name == "tests" => extendedContext }
      .flatMap(_.content.generators)
      .map { generator =>
        GeneratorMeta(
          fullName = generator.fullName,
          generics = generator.generics,
          typ = generator.typ,
          parameters = generator.parameters
        )
      }
  }
}
