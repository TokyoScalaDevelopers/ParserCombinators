package com.github.TokyoScalaDevelopers.SimpleXML

import scala.util.parsing.combinator.RegexParsers

case class Element(tag: String, attrs: Map[String, String] = Map(), content: List[Element] = List()) {
    def flatten(pretty: Boolean = true, depth: Int = 0): String = {
        def flattenAttrs(attrs: Map[String, String]) = attrs.map { case (key, value) => key + "=" + "\"" + value + "\"" } mkString(" ")
        def indent(s: String) = if(pretty) { "    " * depth + s } else { s }
        def makeTag(tag: String, attrs: Map[String, String], selfClosing: Boolean = false) = {
            val _attrs = if(attrs.isEmpty) { "" } else { " " + flattenAttrs(attrs) }
            val _selfclosing = if(selfClosing) { " /" } else { "" }

            "<%s%s%s>" format (tag, _attrs, _selfclosing)
        }
        def makeClosingTag(tag: String) = "</%s>" format tag

        val newline = if(pretty) { "\n" } else { "" }
        val flatContent = content.map { _.flatten(pretty=pretty, depth=depth + 1) } mkString(newline)

        if(content.isEmpty) {
            indent(makeTag(tag, attrs, selfClosing=true))
        } else {
            indent(makeTag(tag, attrs)) + newline + flatContent + newline + indent(makeClosingTag(tag))
        }
    }
}

// Here's our pretty basic XML parser! It supports nested elements, elements with attributes, and escaped quotes in attributes.
// We heavily rely on the method ```implicit def regex(r: Regex): Parser[String]```, which converts all bare regular expressions
// into Parser[String]s.
object XMLParser extends RegexParsers {
    val identifier = "[a-zA-Z0-9]+".r

    val escapedString = "\"" ~> rep("\\\\\"\\s*".r | "\\\\\\s*".r | "[^\\\\\"]+\\s*".r) <~ "\"" ^^ {
        case contents => contents.mkString
    }
    val attribute = identifier ~ "=" ~ ( "[0-9]+".r | "[a-zA-Z0-9]+".r | escapedString ) ^^ {
        case key ~ "=" ~ value => (key, value)
    }

    val attributes = rep(attribute) ^^ {
        case attrs => attrs.toMap
    }

    // <foo attr="val" attr2="escaped \"quotes\"" />
    val selfclosingtag = "<" ~> (identifier ~ attributes) <~ ( "/" ~ ">") ^^ {
        case identifier ~ attrs => Element(identifier, attrs)
    }

    // <foo attr="val">
    val opentag =  "<" ~> (identifier ~ attributes) <~ ">" ^^ {
        case identifier ~ attrs => Element(identifier, attrs)
    }

    // </foo>
    val closetag = ( "<" ~ "/" ) ~> identifier <~ ">" ^^ {
        case identifier => Element(identifier)
    }

    val container: Parser[Element] = ( opentag ~ rep(element) ~ closetag ) ^^ {
        case opentag ~ contents ~ closetag if opentag.tag == closetag.tag => opentag.copy(content=contents)
        case _ => sys.error("Unable to continue, probably malformed XML")
    }

    val element = selfclosingtag | container

    def handleString(xml: String) = parseAll(element, xml) match {
        case Success(result, _) => Some(result)
        case Failure(msg, _) => None
        case Error(msg, _) => None
    }
}
