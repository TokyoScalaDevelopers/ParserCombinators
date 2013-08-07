package test.scala

import se.hardchee.CalculatorParser._

object CalculatorParserTest {
    def main(args: Array[String]) {
        val expr = "1 - 2 + 3 * (4 + 5)"
        println("Evaluating: " + expr)
        val parsed = CalculatorParser.parseExpression(expr)
        println(parsed)
    }
}
