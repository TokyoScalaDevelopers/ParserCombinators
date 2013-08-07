package test.scala

import se.hardchee.CalculatorParser._

object CalculatorParserTest {
    def main(args: Array[String]) {
        val parsed = CalculatorParser.parseExpression("1 - 2 + 3 * (4 + 5)")
        println(parsed)
    }
}
