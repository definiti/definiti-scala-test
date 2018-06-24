package definiti.scalatests.builder.generators

import definiti.common.ast._
import definiti.common.utils.StringUtils
import definiti.scalatests.builder.BuilderContext
import definiti.scalatests.builder.common.GeneratorsExtractor
import definiti.scalatests.{Configuration, ast => scalaAst}
import definiti.tests.ast.GeneratorMeta
import definiti.tests.validation.helpers.ScopedExpression
import definiti.tests.{CoreGenerators, ast => testsAst}

class ScalaGeneratorsAstBuilder(config: Configuration, library: Library) {
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
    val contexts = namespace.elements.collect {
      case extendedContext: ExtendedContext[testsAst.TestsContext] if extendedContext.name == "tests" => extendedContext
    }
    val statements = contexts.flatMap { context =>
      context.content.generators.map(GeneratorBuilder.buildGenerator)
    }
    val namespaceName = if (namespace.fullName.isEmpty) "root" else namespace.fullName
    scalaAst.Namespace(
      name = namespaceName,
      imports = defaultImports(),
      elements = Seq(scalaAst.ObjectDef(
        name = s"${StringUtils.lastPart(namespaceName).capitalize}Generators",
        extendz = Seq.empty,
        body = statements
      ))
    )
  }

  private def defaultImports(): Seq[scalaAst.Import] = Seq(
    scalaAst.Import("org.scalacheck.Gen"),
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

  def scopedExpression(expression: testsAst.Expression, generators: Seq[GeneratorMeta]): ScopedExpression[testsAst.Expression] = {
    new ScopedExpression[testsAst.Expression](expression, Map.empty, Seq.empty, generators, library)
  }

  def isNative(expression: ScopedExpression[_ <: testsAst.Expression]): Boolean = {
    library.typesMap.get(expression.typeOfExpression.name).collect { case x: NativeClassDefinition => x }.isDefined
  }
}
