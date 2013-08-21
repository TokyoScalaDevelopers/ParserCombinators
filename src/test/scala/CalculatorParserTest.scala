package test.scala

import se.hardchee.CalculatorParser._

object CalculatorParserTest {
    def parse(expr: String) = {
        println("Parsing: " + expr)
        CalculatorParser.parseExpression(expr)
    }
    def evaluate(expr: Component) = {
        println("Evaluating: " + expr)
        val r = CalculatorEvaluator.evaluate(expr)
        println("Result: " + r)
        r
    }

    def calc(arg: Any) = arg match {
        case expr: String => parse(expr).flatMap(evaluate)
        case comp: Component => evaluate(comp)
        case x => sys.error("Not sure how to calculate " + x)
    }

    def main(args: Array[String]) {
        goodExpression
        bogusExpression
    }

    def goodExpression {
        calc("1 - 2 + 3 * (4 + 5)")
    }

    def bogusExpression {
        calc(Terms(List( Number(1.0), Operator("*") )))
    }
}
