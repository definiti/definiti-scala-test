package definiti.scalatests.builder.common

import definiti.scalamodel.utils.StringUtils
import definiti.scalatests.builder.BuilderContext
import definiti.scalatests.{ast => scalaAst}
import definiti.tests.validation.helpers.ScopedExpression
import definiti.tests.{ast => testsAst}

object GenExpressionBuilder {
  def buildGenExpression(scoped: ScopedExpression[testsAst.Expression])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    if (isGenerator(scoped.expression) || scoped.isGeneratorExpression) {
      scoped.expression match {
        case boolean: testsAst.BooleanExpression => constGen(scalaAst.Value(boolean.value.toString))
        case number: testsAst.NumberExpression => constGen(scalaAst.Value(number.value.toString()))
        case string: testsAst.StringExpression => constGen(scalaAst.StringExpression(string.value.substring(1, string.value.length - 1)))
        case reference: testsAst.Reference => buildReferenceExpression(ScopedExpression(reference, scoped))
        case generation: testsAst.GenerationExpression => buildGenerationExpression(ScopedExpression(generation, scoped))
        case structure: testsAst.StructureExpression => buildGenStructure(ScopedExpression(structure, scoped))
        case methodCall: testsAst.MethodCall => buildGenMethodCall(ScopedExpression(methodCall, scoped))
        case attributeCall: testsAst.AttributeCall => buildGenAttributeCall(ScopedExpression(attributeCall, scoped))
        case condition: testsAst.Condition => buildGenCondition(ScopedExpression(condition, scoped))
        case binary: testsAst.Binary => buildGenBinary(ScopedExpression(binary, scoped))
      }
    } else {
      constGen(ExpressionBuilder.buildSimpleExpression(scoped))
    }
  }

  def isGenerator(expression: testsAst.Expression): Boolean = {
    expression match {
      case _: testsAst.BooleanExpression => false
      case _: testsAst.NumberExpression => false
      case _: testsAst.StringExpression => false
      case _: testsAst.GenerationExpression => true
      case structure: testsAst.StructureExpression =>
        structure.fields.exists(field => isGenerator(field.expression))
      case _: testsAst.Reference => false
      case methodCall: testsAst.MethodCall =>
        isGenerator(methodCall.inner) || methodCall.arguments.exists(isGenerator)
      case attributeCall: testsAst.AttributeCall =>
        isGenerator(attributeCall.inner)
      case condition: testsAst.Condition =>
        Seq(condition.condition, condition.thenCase, condition.elseCase).exists(isGenerator)
      case binary: testsAst.Binary =>
        isGenerator(binary.left) || isGenerator(binary.right)
    }
  }

  private def constGen(expression: scalaAst.Expression): scalaAst.Expression = {
    scalaAst.CallMethod(
      target = scalaAst.Value("Gen"),
      name = "const",
      arguments = Seq(expression),
      generics = Seq.empty
    )
  }

  private def buildReferenceExpression(reference: ScopedExpression[testsAst.Reference])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    if (reference.isGeneratorExpression) {
      scalaAst.Value(reference.target)
    } else {
      constGen(scalaAst.Value(reference.target))
    }
  }

  private def buildGenerationExpression(generation: ScopedExpression[testsAst.GenerationExpression])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    if (generation.arguments.nonEmpty) {
      val ignore = scalaAst.ForComprehensionCase("_", "<-", scalaAst.Value("initializeGenContext"))
      val arguments = generateArgumentExpressions(generation)
      val generationCall = scalaAst.ForComprehensionCase(
        name = "result",
        operator = "<-",
        body = scalaAst.CallFunction(
          target = generatorName(generation.expression),
          arguments = generation.arguments.indices.map(index => scalaAst.Value(s"arg${index}")),
          generics = generation.generics.map(TypeBuilder.buildType)
        )
      )
      scalaAst.ForComprehension(
        cases = ignore +: arguments :+ generationCall,
        yielding = scalaAst.Value("result")
      )
    } else {
      scalaAst.CallFunction(
        target = generatorName(generation.expression),
        arguments = Seq.empty,
        generics = generation.generics.map(TypeBuilder.buildType)
      )
    }
  }

  private def generateArgumentExpressions(
    generation: ScopedExpression[testsAst.GenerationExpression]
  )(implicit builderContext: BuilderContext): Seq[scalaAst.ForComprehensionCase] = {
    extractArgumentParameterIndex(generation).map { case (argument, parameter, index) =>
      val operator = if (parameter.isGen) "=" else "<-"
      scalaAst.ForComprehensionCase(s"arg${index}", operator, buildGenExpression(argument))
    }
  }

  private def extractArgumentParameterIndex(
    generation: ScopedExpression[testsAst.GenerationExpression]
  )(implicit builderContext: BuilderContext): Seq[(ScopedExpression[testsAst.Expression], testsAst.Parameter, Int)] = {
    val parameters = builderContext.generator(generation.name).parameters
    generation.arguments
      .zipWithIndex
      .map { case (argument, index) =>
        (argument, parameters.applyOrElse(index, (_: Int) => parameters.last), index)
      }
  }

  private def generatorName(generation: testsAst.GenerationExpression)(implicit builderContext: BuilderContext): scalaAst.Expression = {
    if (builderContext.coreGenerators.exists(_.fullName == generation.name)) {
      scalaAst.Value(generation.name)
    } else {
      val nameWithoutNamespace = StringUtils.lastPart(generation.name)
      val namespace = StringUtils.excludeLastPart(generation.name)
      val lastNamespace = StringUtils.lastPart(namespace)
      val generationName = s"${namespace}.${lastNamespace.capitalize}Generators.${nameWithoutNamespace}"
      scalaAst.Value(generationName)
    }
  }

  private def buildGenStructure(structure: ScopedExpression[testsAst.StructureExpression])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    val generators = structure.fields.map(field => buildGenExpression(field.expression))
    val forComprehensionCases = structure.fields.zip(generators).map { case (field, expression) =>
      scalaAst.ForComprehensionCase(
        name = field.name,
        operator = "<-",
        body = expression
      )
    }
    val callCaseClass = scalaAst.CallCaseClass(
      name = structure.typ.name,
      arguments = structure.fields.map(field => scalaAst.NamedArgument(field.name, scalaAst.Value(field.name)))
    )
    scalaAst.ForComprehension(
      cases = forComprehensionCases,
      yielding = callCaseClass
    )
  }

  private def buildGenMethodCall(methodCall: ScopedExpression[testsAst.MethodCall])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    val inner = scalaAst.ForComprehensionCase(
      name = "inner",
      operator = "<-",
      body = buildGenExpression(methodCall.inner)
    )
    val arguments = methodCall.arguments.zipWithIndex.map { case (argument, index) =>
      scalaAst.ForComprehensionCase(
        name = s"arg${index}",
        operator = "<-",
        body = buildGenExpression(argument)
      )
    }
    val callMethod = {
      if (ExpressionBuilder.isNative(methodCall.inner)) {
        scalaAst.CallMethod(
          target = scalaAst.Value(s"${methodCall.inner.typeOfExpression.name}Extension"),
          name = methodCall.method,
          arguments = scalaAst.Value("inner") +: methodCall.arguments.indices.map(index => scalaAst.Value(s"arg${index}")),
          generics = methodCall.generics.map(TypeBuilder.buildType)
        )
      } else {
        scalaAst.CallMethod(
          target = scalaAst.Value("inner"),
          name = methodCall.method,
          arguments = methodCall.arguments.indices.map(index => scalaAst.Value(s"arg${index}")),
          generics = methodCall.generics.map(TypeBuilder.buildType)
        )
      }
    }
    scalaAst.ForComprehension(
      cases = inner +: arguments,
      yielding = callMethod
    )
  }

  private def buildGenAttributeCall(attributeCall: ScopedExpression[testsAst.AttributeCall])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    val inner = scalaAst.ForComprehensionCase(
      name = "inner",
      operator = "<-",
      body = buildGenExpression(attributeCall.inner)
    )
    val callAttribute = {
      if (ExpressionBuilder.isNative(attributeCall.inner)) {
        scalaAst.CallMethod(
          target = scalaAst.Value(s"${attributeCall.inner.typeOfExpression.name}Extension"),
          name = attributeCall.attribute,
          arguments = Seq(scalaAst.Value("inner")),
          generics = Seq.empty
        )
      } else {
        scalaAst.CallAttribute(
          target = scalaAst.Value("inner"),
          name = attributeCall.attribute
        )
      }
    }
    scalaAst.ForComprehension(
      cases = Seq(inner),
      yielding = callAttribute
    )
  }

  private def buildGenCondition(condition: ScopedExpression[testsAst.Condition])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    val conditionCase = scalaAst.ForComprehensionCase("condition", "<-", buildGenExpression(condition.condition))
    val thenCase = scalaAst.ForComprehensionCase("thenCase", "<-", buildGenExpression(condition.thenCase))
    val elseCase = scalaAst.ForComprehensionCase("elseCase", "<-", buildGenExpression(condition.elseCase))
    val yielding = scalaAst.IfThenElse(
      cond = scalaAst.Value("condition"),
      ifTrue = scalaAst.Value("thenCase"),
      ifFalse = scalaAst.Value("elseCase")
    )
    scalaAst.ForComprehension(
      cases = Seq(conditionCase, thenCase, elseCase),
      yielding = yielding
    )
  }

  private def buildGenBinary(binary: ScopedExpression[testsAst.Binary])(implicit builderContext: BuilderContext): scalaAst.Expression = {
    val left = scalaAst.ForComprehensionCase("left", "<-", buildGenExpression(binary.left))
    val right = scalaAst.ForComprehensionCase("right", "<-", buildGenExpression(binary.right))
    val yielding = scalaAst.BinaryOp(ExpressionBuilder.binaryOperatorToString(binary.operator), scalaAst.Value("left"), scalaAst.Value("right"))
    scalaAst.ForComprehension(
      cases = Seq(left, right),
      yielding = yielding
    )
  }
}
