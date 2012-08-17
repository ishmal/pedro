/**
 * Test XSLT capabilities
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2012 Bob Jamison
 * 
 *  This file is part of the Pedro library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 3 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

package pedro.data

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec}
import org.scalatest.matchers.{ShouldMatchers}

import pedro.net.servlet._


@RunWith(classOf[JUnitRunner])
class XsltTest extends FeatureSpec with ShouldMatchers
{
    feature("The XsltFilter must be able to transform xml")
        {
        scenario("The filter converts a Docbook doc to html")
            {
            val filter = new XsltFilter
            val xslName = "src/test/resources/doc2html.xsl"
            filter.templates = filter.loadFile(xslName).flatMap(filter.loadTemplate)
            assert(filter.templates.isDefined)
            val docName = "src/test/resources/crystalegg.docbook"
            val inbuf = scala.io.Source.fromFile(docName)("UTF-8").mkString
            val result = filter.transform(inbuf.getBytes)
            assert(result.isDefined)
            info(new String(result.get))
            }

    
        }

}


