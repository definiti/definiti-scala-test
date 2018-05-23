package definiti.scalatests.builder.generators

import definiti.common.ast.Library
import definiti.scalatests.builder.common.{GenExpressionBuilder, TypeBuilder}
import definiti.scalatests.{ast => scalaAst}
import definiti.tests.ast.{GeneratorMeta, Type}
import definiti.tests.validation.helpers.ScopedExpression
import definiti.tests.{ast => testsAst}

object GeneratorBuilder {

  def buildGenerator(generator: testsAst.Generator, generators: Seq[GeneratorMeta])(implicit library: Library, coreGenerators: Seq[GeneratorMeta]): scalaAst.Statement = {
    scalaAst.Def(
      name = generator.name,
      typ = TypeBuilder.buildType(Type("Gen", generator.typ)),
      body = GenExpressionBuilder.buildGenExpression(generatorScope(generator, generators)),
      generics = generator.generics,
      parameters = generator.parameters.map(buildParameter)
    )
  }

  private def generatorScope(generator: testsAst.Generator, generators: Seq[GeneratorMeta])(implicit library: Library): ScopedExpression[testsAst.Expression] = {
    new ScopedExpression(
      expression = generator.expression,
      references = {
        generator.parameters.map { parameter =>
          if (parameter.isRest) {
            parameter.name -> Type("List", Seq(parameter.typ))
          } else {
            parameter.name -> parameter.typ
          }
        }.toMap
      },
      generators = generators,
      library = library
    )
  }

  private def buildParameter(parameter: testsAst.Parameter): scalaAst.Parameter = {
    scalaAst.Parameter(
      name = parameter.name,
      typ = TypeBuilder.buildType(parameter.typ)
    )
  }
}
