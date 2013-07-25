package se.hardchee

import scala.util.parsing.combinator.RegexParsers

case class Element(tag: String, attrs: Map[String, String] = Map(), content: List[Element] = List())

object XMLParser extends RegexParsers {
    val identifier = "[a-zA-Z0-9]+".r
    val attributes = "foo=\"bar\""

    val selfclosingtag = "<" ~> (identifier ~ rep(attributes)) <~ ( "/" ~ ">") ^^ {
        case identifier ~ attributes => Element(identifier)
    }
    val opentag =  "<" ~> (identifier ~ rep(attributes)) <~ ">" ^^ {
        case identifier ~ attributes => Element(identifier)
    }
    val closetag = ( "<" ~ "/" ) ~> identifier <~ ">" ^^ {
        case identifier => Element(identifier)
    }

    val element: Parser[Any] = selfclosingtag | ( opentag ~ element ~ closetag )

    def handleString(xml: String) = parseAll(element, xml)
}

object SimpleXML {
    def main(args: Array[String]) {
        println(XMLParser.handleString("<test><foo /></blah>"))
    }
}
