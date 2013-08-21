package test.scala

import com.github.TokyoScalaDevelopers.SimpleXML._

object SimpleXMLTests {
    def main(args: Array[String]) {
        val goodXML = """<test attr = "this is another test" test2="I wonder if escaped \"quotes\" work">
                        |  <foo attr="val" attr2="escaped \"quotes\"" />
                        |  <bar>
                        |    <baz attr="this is a test" bareattr=noquotes number=12345 / >
                        |  </bar>
                        |</test>
                        |""".stripMargin
        val badXML =  """<test>
                        |   <foo />
                        |</blah>
                        |""".stripMargin

        val a: Element = XMLParser.handleString(goodXML)
        println(a)
        println(a.flatten(pretty=true))
        println(a.flatten(pretty=false))
    }
}
