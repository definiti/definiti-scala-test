package definiti.scalatests

import java.nio.file.Path

import definiti.common.ast.{Library, Root}
import definiti.common.plugin.GeneratorPlugin
import definiti.scalatests.builder.generators.ScalaGeneratorsAstBuilder
import definiti.scalatests.builder.tests.ScalaTestAstBuilder
import definiti.scalatests.generator.ScalaTestsProjectGenerator

import scala.io.Source

class ScalaTestsGeneratorPlugin extends GeneratorPlugin {
  private val config = new FileConfiguration()

  override def name: String = "scala-tests-generator"

  override def generate(root: Root, library: Library): Map[Path, String] = {
    nativeSources ++ generatedSources(root, library)
  }

  def nativeSources: Map[Path, String] = {
    val content = Source.fromResource("scalatests/native.scala").mkString
    Map(config.destination.resolve("definiti/scalatests/native/native.scala") -> content)
  }

  def generatedSources(root: Root, library: Library): Map[Path, String] = {
    val testsRoot = new ScalaTestAstBuilder(config, library).build(root)
    val generatorsRoot = new ScalaGeneratorsAstBuilder(config, library).build(root)
    new ScalaTestsProjectGenerator(config).generateProject(testsRoot, generatorsRoot)
  }
}
