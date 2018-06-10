package definiti.scalatests.builder

import definiti.common.ast.Library
import definiti.tests.ast.GeneratorMeta

case class BuilderContext(
  library: Library,
  coreGenerators: Seq[GeneratorMeta],
  projectGenerators: Seq[GeneratorMeta]
) {
  private val generatorMap: Map[String, GeneratorMeta] = {
    (coreGenerators ++ projectGenerators)
      .map(generator => generator.fullName -> generator)
      .toMap
  }

  val generators: Seq[GeneratorMeta] = coreGenerators ++ projectGenerators

  def generator(name: String): GeneratorMeta = generatorMap(name)
}