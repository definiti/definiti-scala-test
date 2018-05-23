package definiti.scalatests.builder.common

import definiti.common.ast.{ExtendedContext, Root}
import definiti.tests.ast.GeneratorMeta
import definiti.tests.{ast => testsAst}

object GeneratorsExtractor {

  def extractGenerators(root: Root): Seq[testsAst.GeneratorMeta] = {
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
