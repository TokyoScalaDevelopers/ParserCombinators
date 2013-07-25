package test.scala

import se.hardchee.SimpleXML._

object SimpleXMLTests {
    def main(args: Array[String]) {
        val goodXML = """<test attr = "this is another test" test2="I wonder if escaped \"quotes\" work">
                        |  <foo attr="val" attr2="escaped \"quotes\"" />
                        |  <bar>
                        |    <baz attr="this is a test" />
                        |  </bar>
                        |</test>
                        |""".stripMargin
        val badXML =  """<test>
                        |   <foo />
                        |</blah>
                        |""".stripMargin

        val a: Element = XMLParser.handleString(goodXML)
        println(a.flatten())
    }
}
