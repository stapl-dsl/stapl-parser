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

object RemotePolicyTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}
/**
 * Correctness tests of the e-docs policy.
 */
class RemotePolicyTest extends AssertionsForJUnit {

  import EdocsAttributes._

  val policyFile = Source.fromURL(getClass.getResource("/edocs.stapl")).mkString
  val parser = new PolicyParser(policyFile, subject, action, resource, environment)
  val policy = parser.Stapl.run() match {
    case Success(result) => result
    case Failure(e: ParseError) => print(parser.formatError(e)); sys.error("wrong input")
    case Failure(e) => e.printStackTrace(); sys.error("wrong input")
  }
  
  // set up the PDP, use an empty attribute finder since we will provide all attributes in the request
  //val pdp = new PDP(javaLikePolicy, new AttributeFinder)
  val pdp = new PDP(policy, new AttributeFinder)

  @Before def setup() {
    
  }
  
  @Test def testOuterRemotePolicy() {
    val policyString = """RemotePolicy("remoteId")"""
    val policy = PolicyParser.parse(policyString, Nil)
    
    assert(policy === RemotePolicy("remoteId"))
  }
  
  @Test def testInnerRemotePolicy() {
    val policyString = 
      """Policy("test") := apply PermitOverrides to (
           RemotePolicy("remoteId1"),
           RemotePolicy("remoteId2")
      )"""
    val policy = PolicyParser.parse(policyString, Nil).asInstanceOf[Policy]
    
    assert(policy.subpolicies === List(RemotePolicy("remoteId1"), RemotePolicy("remoteId2")))
  }
  
}