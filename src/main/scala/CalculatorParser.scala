package se.hardchee.CalculatorParser

import scala.util.parsing.combinator.RegexParsers

sealed trait Component

case class Terms(terms: List[Component]) extends Component
case class Factors(factors: List[Component]) extends Component
case class Operator(op: String) extends Component
case class Number(value: Double) extends Component

object CalculatorParser extends RegexParsers {
    import scala.language.implicitConversions
    implicit def stringToDouble(s: String): Number = Number(s.toDouble)

    val number = "[1-9][0-9]*(?:.[0-9]+)?".r ^^ {
        case num => Number(num.toDouble)
    }


    val opMultiplyDivide = "[*/]".r ^^ { case op => Operator(op) }
    val opAddSubtract = "[+-]".r ^^ { case op => Operator(op) }

    val opMultiplyDividepair = opMultiplyDivide ~ factor ^^ {
        case op ~ factor => List(op, factor)
    }
    val opAddSubtractpair = opAddSubtract ~ term ^^ {
        case op ~ term => List(op, term)
    }

    val parens: Parser[Component] = "(" ~> expression <~ ")"
    val factor: Parser[Component] = number | parens
    val term: Parser[Component] = factor ~ rep( opMultiplyDividepair ) ^^ {
        case factor ~ factors if(factors.isEmpty) => factor
        case factor ~ factors => Factors(factor +: ( factors.flatten ) )
    }

    val expression: Parser[Component] = term ~ rep( opAddSubtractpair ) ^^ {
        case term ~ terms if(terms.isEmpty) => term
        case term ~ terms => Terms( term +: ( terms.flatten ) )
    }

    def parseExpression(s: String) = {
        parseAll(expression, s) match {
            case Success(r, _) => r
            case Failure(m, _) => sys.error("Unable to parse expression! " + m)
            case Error(e, _) => sys.error("Unable to parse expression! " + e)
        }
    }
}
