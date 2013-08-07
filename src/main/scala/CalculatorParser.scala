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

object CalculatorEvaluator {
    def evaluateOp(_lhs: Component, op: Operator, _rhs: Component): Option[Number] = {
        for(lhs <- evaluate(_lhs);
            rhs <- evaluate(_rhs)) yield {
            (lhs, op, rhs) match {
                case (Number(l), Operator("+"), Number(r)) => Number(l + r)
                case (Number(l), Operator("-"), Number(r)) => Number(l - r)
                case (Number(l), Operator("*"), Number(r)) => Number(l * r)
                case (Number(l), Operator("/"), Number(r)) => Number(l / r)
                case (_, Operator(o), _) => sys.error("Unable to handle operator %s.".format(o))
            }
        }
    }

    def evaluateList(xs: List[Component]): Option[Number] = {
        // _work accepts a reversed list! Make sure to flip arguments to evaluate methods!
        def _work(xs: List[Component]): Option[Number] = xs match {
            case (lhs: Component) :: (op: Operator) :: (xs: List[Component]) => _work(xs) flatMap {
                rhs => evaluateOp(rhs, op, lhs)
            }
            case (c: Component) :: Nil => evaluate(c)
            case _ => None
        }
        _work(xs.reverse)
    }

    def evaluate(expression: Component): Option[Number] = {
        expression match {
            case Terms(xs) => evaluateList(xs)
            case Factors(xs) => evaluateList(xs)
            case x:Number => Some(x)
            case x:Operator => None
        }
    }
}
