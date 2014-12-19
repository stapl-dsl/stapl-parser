/**
 *    Copyright 2014 KU Leuven Research and Developement - iMinds - Distrinet
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 *    Administrative Contact: dnet-project-office@cs.kuleuven.be
 *    Technical Contact: maarten.decat@cs.kuleuven.be
 *    Author: maarten.decat@cs.kuleuven.be
 */
package stapl.parser

import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import stapl.core.pdp.PDP
import stapl.core.pdp.AttributeFinder
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit
import stapl.core._
import scala.io.Source
import scala.util.Success
import EdocsAttributes.action
import EdocsAttributes.environment
import EdocsAttributes.resource
import EdocsAttributes.subject
import org.parboiled2.ParserInput.apply
import scala.util.Failure
import org.parboiled2.ParseError

object AttributesTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}

class AttributesTest extends AssertionsForJUnit {

  @Before def setup() {
    // nothing to do
  }

  @Test def testParseAttributes() {
    val parser = new AttributesParser(
      """
      subject.name = SimpleAttribute(String)
      resource.names = ListAttribute(String)
      environment.nrnames = SimpleAttribute ("nr-names",  Number )
      action.day = SimpleAttribute(DayDuration)
      """)
    val attributes = parser.AttributeDefs.run() match {
      case Success(result) => result
      case Failure(e: ParseError) => print(parser.formatError(e)); sys.error("wrong input")
      case Failure(e) => e.printStackTrace(); sys.error("wrong input")
    }
    
    
    assert(
        attributes 
          === 
        Seq(("subject.name", SimpleAttribute(SUBJECT, "name", String)),
            ("resource.names",ListAttribute(RESOURCE, "names", String)), 
            ("environment.nrnames",SimpleAttribute(ENVIRONMENT, "nr-names", Number)), 
            ("action.day",SimpleAttribute(ACTION, "day", DayDuration)))
    )
  }

}