package se.hardchee

import scala.util.parsing.combinator.RegexParsers

object XMLParser extends RegexParsers {
    val identifier = "test"
    val attributes = "foo=\"bar\""
    val tag = "<" ~> (identifier ~ rep(attributes)) <~ ">"

    def handleString(xml: String) = parseAll(tag, xml)
}

object SimpleXML {
    def main(args: Array[String]) {
        println(XMLParser.handleString("<test>"))
    }
}
