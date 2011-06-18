package pedro.data

import pedro.net.servlet._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec,GivenWhenThen}
 

@RunWith(classOf[JUnitRunner])
class XsltSpec extends FeatureSpec with GivenWhenThen
{
    feature("The XsltFilter must be able to transform xml")
        {
        scenario("The filter converts a Docbook doc to html")
            {
            given("an xslt filter")
            val filter = new XsltFilter
            when("the xsl template for docbook is loaded")
            val xslName = "src/test/resources/doc2html.xsl"
            filter.templates = filter.loadFile(xslName).flatMap(filter.loadTemplate)
            then("templates should be defined")
            assert(filter.templates.isDefined)
            val docName = "src/test/resources/crystalegg.docbook"
            val inbuf = scala.io.Source.fromFile(docName)("UTF-8").mkString
            when("transform is performed")
            val result = filter.transform(inbuf.getBytes)
            then("the return value should be defined")
            assert(result.isDefined)
            println(new String(result.get))
            }

    
        }

}


