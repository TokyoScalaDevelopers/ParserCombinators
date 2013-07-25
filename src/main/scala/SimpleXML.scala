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

    val container: Parser[Element] = ( opentag ~ rep(element) ~ closetag ) ^^ {
        case opentag ~ contents ~ closetag if opentag.tag == closetag.tag => opentag.copy(content=contents)
        case _ => sys.error("Unable to continue, probably malformed XML")
    }

    val element = selfclosingtag | container

    def handleString(xml: String) = parseAll(element, xml)
}

object SimpleXML {
    def main(args: Array[String]) {
        val goodXML = """<test>
                        |  <foo />
                        |  <bar>
                        |    <baz />
                        |  </bar>
                        |</test>
                        |""".stripMargin
        println(XMLParser.handleString(goodXML))
        // This will fail (yay!)
        // println(XMLParser.handleString("<test><foo /></blah>"))
    }
}
