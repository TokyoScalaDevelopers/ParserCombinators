package se.hardchee

import scala.util.parsing.combinator.RegexParsers

object XMLParser extends RegexParsers {
    val identifier = "test"
    val attributes = "foo=\"bar\""

    val selfclosingtag = ( "<" ) ~> (identifier ~ rep(attributes)) <~ ( "/" ~ ">")
    val opentag = "<" ~> (identifier ~ rep(attributes)) <~ ">"
    val closetag = "<" ~> (identifier ~ rep(attributes)) <~ ">"

    val element: Parser[Any] = selfclosingtag | ( opentag ~ element ~ closetag )

    def handleString(xml: String) = parseAll(element, xml)
}

object SimpleXML {
    def main(args: Array[String]) {
        println(XMLParser.handleString("<test / >"))
    }
}
