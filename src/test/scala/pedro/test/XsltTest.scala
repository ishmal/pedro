package pedro.test

import pedro.net.servlet._

import org.scalatest.{FeatureSpec,GivenWhenThen}
import org.scalatest.matchers.MustMatchers
 


class XsltSpec extends FeatureSpec with GivenWhenThen with MustMatchers
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
            filter.templates.isDefined must be === true
            val docName = "src/test/resources/crystalegg.docbook"
            val inbuf = scala.io.Source.fromFile(docName)("UTF-8").mkString
            when("transform is performed")
            val result = filter.transform(inbuf.getBytes)
            then("the return value should be defined")
            result.isDefined must be === true
            println(new String(result.get))
            }

    
        }

}


