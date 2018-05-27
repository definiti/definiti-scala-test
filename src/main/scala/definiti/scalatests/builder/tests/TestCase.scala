package definiti.scalatests.builder.tests

import definiti.tests.{ast => testsAst}

case class TestCase(
  testCase: testsAst.Case,
  subCase: testsAst.SubCase
)