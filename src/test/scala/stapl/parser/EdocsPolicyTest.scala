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

object EdocsPolicyTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}
/**
 * Correctness tests of the e-docs policy.
 */
class EdocsPolicyTest extends AssertionsForJUnit {

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
    // nothing to do
  }

  @Test def testDenyHelpdeskNotAssigned() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.role -> List("helpdesk"),
        subject.tenant_name -> List("provider"),
        subject.tenant_type -> List("provider"),
        subject.assigned_tenants -> List("tenant1","tenant3"),
        resource.type_ -> "document",
        resource.owning_tenant -> "tenant4",
        resource.confidential -> false) === Result(Deny, List()))
  }

  @Test def testPermitHelpdeskAssigned() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.role -> List("helpdesk"),
        subject.tenant_name -> List("provider"),
        subject.tenant_type -> List("provider"),
        subject.assigned_tenants -> List("tenant1","tenant3"),
        resource.type_ -> "document",
        resource.owning_tenant -> "tenant3",
        resource.confidential -> false) === Result(Permit, List()))
  }

  @Test def testPermitLargeBankCreateSubtenant() {
    assert(pdp.evaluate("maarten", "create", "subtenantX",
        subject.role -> List("senior"),
        subject.department -> "IT",
        subject.tenant -> "large-bank",
        subject.tenant_name -> List("large-bank"),
        subject.tenant_type -> List("tenant"),
        resource.type_ -> "subtenant",
        resource.confidential -> false,
        resource.owning_tenant -> "large-bank") === Result(Permit, List()))
  }

  @Test def testDenyLargeBankCreateSubtenantWrongDepartment() {
    assert(pdp.evaluate("maarten", "create", "subtenantX",
        subject.role -> List("senior"),
        subject.department -> "another-department",
        subject.tenant -> "large-bank",
        subject.tenant_name -> List("large-bank"),
        subject.tenant_type -> List("tenant"),
        resource.type_ -> "subtenant",
        resource.confidential -> false,
        resource.owning_tenant -> "large-bank") === Result(Deny, List()))
  }

  @Test def testDenyLargeBankCreateSubtenantWrongRole() {
    assert(pdp.evaluate("maarten", "create", "subtenantX",
        subject.role -> List("junior"),
        subject.department -> "another-department",
        subject.tenant -> "large-bank",
        subject.tenant_name -> List("large-bank"),
        subject.tenant_type -> List("tenant"),
        resource.type_ -> "subtenant",
        resource.confidential -> false,
        resource.owning_tenant -> "large-bank") === Result(Deny, List()))
  }
}

object EdocsAttributes extends BasicPolicy{
  subject.assigned_offices          = ListAttribute(String)
  subject.assigned_tenants          = ListAttribute(String)
  subject.customers_of_bank_office      = ListAttribute(String)
  subject.customers_of_direct_tenant    = ListAttribute(String)
  subject.delegated_view_from         = ListAttribute(String)
  subject.department            = SimpleAttribute(String)
  subject.projects              = ListAttribute(String)
  subject.responsible_for           = ListAttribute(String)
  subject.role                = ListAttribute(String)
  subject.supervisees             = ListAttribute(String)
  subject.tenant              = SimpleAttribute(String)
  subject.tenant_credit_sufficient      = SimpleAttribute(Bool)
  subject.tenant_name             = ListAttribute(String)
  subject.tenant_type             = ListAttribute(String)
  resource.confidential           = SimpleAttribute(Bool)
  resource.contains_personal_information  = SimpleAttribute(Bool)
  resource.creating_tenant_name       = SimpleAttribute(String)
  resource.delegated_view           = ListAttribute(String)
  resource.destination            = SimpleAttribute(String)
  resource.destination_customer_type    = SimpleAttribute(String)
  resource.destination_department       = SimpleAttribute(String)
  resource.destination_office         = SimpleAttribute(String)
  resource.destination_owns_savings_account = SimpleAttribute(Bool)
  resource.origin               = SimpleAttribute(String)
  resource.owning_tenant          = SimpleAttribute(String)
  resource.project              = SimpleAttribute(String)
  resource.topic              = SimpleAttribute(String)
  resource.type_              = SimpleAttribute(String)
  environment.current_date_between_20_and_25    = SimpleAttribute(Bool)   // Note: we did not want to model this in XACML in full 
  environment.current_time_between_7_and_19     = SimpleAttribute(Bool)   // because of the difficulty of writing this, but we could do this easily
  environment.dateTimeOk              = SimpleAttribute(Bool)   // in STAPL! However, for honest comparison: leave it this way.
}