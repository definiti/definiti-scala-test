package definiti.scalatests.builder.generators

import definiti.scalatests.builder.BuilderContext
import definiti.scalatests.builder.common.{GenExpressionBuilder, TypeBuilder}
import definiti.scalatests.{ast => scalaAst}
import definiti.tests.validation.helpers.ScopedExpression
import definiti.tests.validation.helpers.ScopedExpression.TypeInfo
import definiti.tests.{ast => testsAst}

object GeneratorBuilder {

  def buildGenerator(generator: testsAst.Generator)(implicit builderContext: BuilderContext): scalaAst.Statement = {
    scalaAst.Def(
      name = generator.name,
      typ = TypeBuilder.buildType(testsAst.Type("Gen", generator.typ)),
      body = GenExpressionBuilder.buildGenExpression(generatorScope(generator)),
      generics = generator.generics,
      parameters = generator.parameters.map(buildParameter)
    )
  }

  private def generatorScope(generator: testsAst.Generator)(implicit builderContext: BuilderContext): ScopedExpression[testsAst.Expression] = {
    new ScopedExpression(
      expression = generator.expression,
      references = {
        generator.parameters.map { parameter =>
          if (parameter.isRest) {
            parameter.name -> TypeInfo(testsAst.Type("List", Seq(parameter.typ)), isGenerator = parameter.isGen)
          } else {
            parameter.name -> TypeInfo(parameter.typ, isGenerator = parameter.isGen)
          }
        }.toMap
      },
      definedGenerics = Seq.empty,
      generators = builderContext.generators,
      library = builderContext.library
    )
  }

  private def buildParameter(parameter: testsAst.Parameter): scalaAst.Parameter = {
    scalaAst.Parameter(
      name = parameter.name,
      typ = typeOfParameter(parameter)
    )
  }

  private def typeOfParameter(parameter: testsAst.Parameter): scalaAst.Type = {
    if (parameter.isGen) {
      scalaAst.Type("Gen", Seq(TypeBuilder.buildType(parameter.typ)))
    } else {
      TypeBuilder.buildType(parameter.typ)
    }
  }
}
