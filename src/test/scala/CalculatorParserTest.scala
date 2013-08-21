package test.scala

import se.hardchee.CalculatorParser._

object CalculatorParserTest {
    def parse(expr: String) = {
        CalculatorParser.parseExpression(expr)
    }
    def evaluate(expr: Component) = {
        CalculatorEvaluator.evaluate(expr)
    }

    def calc(arg: Any) = arg match {
        case expr: String => parse(expr).flatMap(evaluate)
        case comp: Component => evaluate(comp)
        case x => sys.error("Not sure how to calculate " + x)
    }

    def main(args: Array[String]) {
      val expressions = List(
        ("good expression", "1 - 2 + 3 * (4 + 5)", Some(Number(26.0))),
        ("bad expression", Terms(List( Number(1.0), Operator("*") )), None)
      )

      expressions.map({ case (message, expr: Any, expectedResult: Option[Number]) =>
        println("--------------------")
        println("Testing \"%s\": %s".format(message, expr))
        val result = calc(expr)
        val expectedMessage = "%s == %s: %s".format(result.toString, expectedResult.toString, (result == expectedResult).toString )
        println("  Result: %s".format(expectedMessage))
      })
      println("--------------------")
    }
}
