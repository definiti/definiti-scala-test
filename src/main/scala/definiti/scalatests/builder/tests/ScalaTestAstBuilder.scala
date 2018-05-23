package definiti.scalatests.builder.tests

import definiti.common.ast._
import definiti.common.utils.StringUtils
import definiti.scalatests.{Configuration, ast => scalaAst}
import definiti.tests.ast.GeneratorMeta
import definiti.tests.{CoreGenerators, ast => testsAst}

class ScalaTestAstBuilder(config: Configuration, library: Library) {
  implicit def lib: Library = library

  def build(root: Root): scalaAst.Root = {
    implicit val coreGenerators = CoreGenerators.coreGenerators.fold(_ => Seq.empty, identity)
    val generators = extractGenerators(root) ++ coreGenerators
    scalaAst.Root(
      namespaces = root.namespaces
        .map(buildNamespace(_, generators))
        .filter(_.elements.nonEmpty)
    )
  }

  private def buildNamespace(namespace: Namespace, generatorsMeta: Seq[GeneratorMeta])(implicit coreGenerators: Seq[GeneratorMeta]): scalaAst.Namespace = {
    val contexts = namespace.elements.collect {
      case extendedContext: ExtendedContext[testsAst.TestsContext] if extendedContext.name == "tests" => extendedContext
    }
    val statements = contexts.flatMap { context =>
      context.content.tests.flatMap {
        case testVerification: testsAst.TestVerification => TestVerificationBuilder.buildTestVerification(testVerification, generatorsMeta)
        case testType: testsAst.TestType => TestTypeBuilder.buildTestType(testType, generatorsMeta)
      }
    }
    val namespaceName = if (namespace.fullName.isEmpty) "root" else namespace.fullName
    scalaAst.Namespace(
      name = namespaceName,
      imports = defaultImports(),
      elements = Seq(scalaAst.ClassDef(
        name = s"${StringUtils.lastPart(namespaceName).capitalize}Spec",
        extendz = Seq("FlatSpec", "Matchers", "PropertyChecks"),
        body = statements
      ))
    )
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
