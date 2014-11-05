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
import scala.io.Source
import scala.util.Success
import EdocsAttributes.action
import EdocsAttributes.environment
import EdocsAttributes.resource
import EdocsAttributes.subject
import scala.util.Failure
import org.parboiled2.ParseError
import stapl.core.BasicPolicy

object CompleteParserTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}

class CompleteParserTest extends AssertionsForJUnit {
  
  @Before def setup() {
    // nothing to do
  }

  @Test def testParseCompletePolicy() {
    val (s, a, r, e) = BasicPolicy.containers
    val parser = new CompleteParser(TestPolicy.policyString, s, a, r, e)
    val policy = parser.CompletePolicy.run() match {
      case Success(result) => result
      case Failure(e: ParseError) => print(parser.formatError(e)); sys.error("wrong input")
      case Failure(e) => e.printStackTrace(); sys.error("wrong input")
    }
    
    assert(policy.toString === TestPolicy.policy.toString)
  }

  object TestPolicy extends BasicPolicy {
    
    import stapl.core._
    
    val policyString = 
"""
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
  
  Policy("edocs") := apply DenyOverrides to (
      // For members of the provider
      Policy("members-of-provider") := when ("provider" in subject.tenant_name) apply FirstApplicable to (
          
          // No member of the Provider can read a document labeled confidential.
          Rule("confidential") := deny iff ((action.id === "view") & (resource.type_ === "document") & resource.confidential),
          
          // For members of the helpdesk
          Policy("helpdesk") := when ("helpdesk" in subject.role) apply FirstApplicable to (
              
              // Members of the helpdesk can view the metadata of every document in the application.
              Rule("metadata") := permit iff ((action.id === "view") & (resource.type_ === "document_metadata")),
              
              // Members of the helpdesk can only view the contents of document belonging to tenants for which they are assigned responsible.
              Policy("contents") := when ((action.id === "view") & (resource.type_ === "document")) apply PermitOverrides to (
                  Rule("assigned-tenants") := permit iff (resource.owning_tenant in subject.assigned_tenants),
                  Rule("default-deny") := deny
              ),
              
              Rule("default-deny") := deny
          ),
          
          // For application admins
          Policy("admins") := when ("admin" in subject.role) apply FirstApplicable to (
              
              // Admins can create new tenants
              Rule("create-tenant") := permit iff ((action.id === "create") & (resource.type_ === "tenant")),
              
              Rule("default-deny") := deny
          )
      ),
      
      // For unregistered receivers
      Policy("unregistered-receivers") := when ("unregistered_receiver" in subject.role) apply DenyOverrides to (
          
          // Unregistered Receivers can only view stuff and can only view documents and document metadata.
          Policy("view document") := apply PermitOverrides to ( 
              Rule("permit view document") := permit iff ((action.id === "view") & (resource.type_ === "document")),              
              Rule("default-deny") := deny
          ), 
          
          // A Unregistered Receiver can only view documents sent to him/herself.
          Rule("only sent to him/herself") := deny iff ((action.id === "view") & (resource.destination === subject.id)),
          
          Rule("default-permit") := permit
      ),
      
      // For registered private receivers
      Policy("registered private receivers") := when ("registered_private_receiver" in subject.role) apply PermitOverrides to (
          
          // A Registered Private Receiver can only view documents
          Rule("only view documents") := deny iff ((action.id === "view") & (resource.type_ === "document")),
          
          // The three reasons for a permit: own document, delegated document or delegated all documents.
          Policy("view") := when (action.id === "view") apply PermitOverrides to (
              
              // A Registered Private Receiver can view documents which he/she received
              Rule("own documents") := permit iff (resource.destination === subject.id),
              
              // A Registered Private Receiver can view documents which belong to another Registered Receiver which has allowed him/her to read this specific document.
              Rule("delegated specific") := permit iff (subject.id in resource.delegated_view),
              
              // A Registered Private Receiver can view documents belonging to another Registered Private Receiver which has allowed him/her to read all his/her documents.
              Rule("delegated all") := permit iff (resource.destination in subject.delegated_view_from)
          ),
          
          Rule("default-permit") := permit
      ),
      
      // For tenants in general
      Policy("tenants") := when ("tenant" in subject.tenant_type) apply DenyOverrides to (
          
          // A member of a Tenant can only send a document if the credit of that tenant is sufficient.
          Rule("credit") := deny iff ((action.id === "send") & (resource.type_ === "document") & !subject.tenant_credit_sufficient),
          
          // Tenant isolation
          Rule("tenant isolation") := deny iff (! (resource.owning_tenant === subject.tenant))
      ),
      
      // For specific tenants
      // For Large Bank
      Policy("large-bank") := when ("large-bank" in subject.tenant_name) apply DenyOverrides to (
          
          // For general documents
          Policy("documents") := when (resource.type_ === "document") apply DenyOverrides to (
              
              // Every authenticated user can send documents with some restrictions for subtenants and specific types of users.
              Policy("send") := when (action.id === "send") apply DenyOverrides to (
                  
                  // Members of a subtenant can only send documents to customers of that subtenant.
                  Rule("only to own subtenant") := deny iff (("subtenant" in subject.tenant_type) & !(resource.owning_tenant in subject.customers_of_direct_tenant)),
                  
                  // Members of a bank office can only send documents to external customers whose main office is that bank office.
                  Rule("bank office") := deny iff (("bank_office" in subject.tenant_type) & !(resource.owning_tenant in subject.customers_of_bank_office)),
                  
                  // Refinement of the previous rule: Insurance agents of a bank office can only send documents to insurance customers of that bank office.
                  Rule("insurance agents") := deny iff (("bank_office" in subject.tenant_type) & ("insurance-agent" in subject.role) & !(resource.destination_customer_type === "insurance")),
                  
                  Rule("default-permit") := permit
              ),
          
            // View documents
            Policy("viewing documents") := when (action.id === "view") apply PermitOverrides to (
                
                // A user can read documents sent to himself/herself
                Rule("only to him/herself") := deny iff (! (resource.destination === subject.id)),
                
                // A supervisor can read documents sent by its supervisees.
                Rule("supervisor") :=  permit iff (("superviser" in subject.role) & (resource.origin in subject.supervisees)),
                
                // A project member can read all documents regarding the project.
                Rule("project members") := permit iff (("project_member" in subject.role) & (resource.project in subject.projects)),
                
                // Members of the Large Bank Audit department can read any document sent by any member of Large Bank, except for the paychecks and banking notes, or any other document marked in its meta-data to contain personal information of the customer.
                Policy("audit") := when ("audit" in subject.role) apply DenyOverrides to (
                    
                    // First reason to deny: not sent by a member of the Large Bank.
                    Rule("not-a-member") := deny iff (! (resource.creating_tenant_name === "large-bank")),
                    
                    // Second reason to deny: paychecks.
                    Rule("paychecks") := deny iff (resource.type_ === "paycheck"), 
                    
                    // Third reason to deny: banking notes.
                    Rule("banking-notes") := deny iff (resource.type_ === "banking_note"),
                    
                    // Fourth reason to deny: marked as containing personal information.
                    Rule("personal-info") := deny iff (resource.contains_personal_information),
                    
                    Rule("default-permit") := permit
                ),
                
                Rule("default-deny") := deny
            )
          ),
          
          // For invoices
          Policy("invoices") := when (resource.type_ === "invoice") apply DenyOverrides to (
              
              // For sending invoices: only members of the sales department can send invoices.
              Policy("sending") := when (action.id === "send") apply DenyOverrides to (
                  Rule("only-sales-department") := deny iff (! (subject.department === "sales")),
                  Rule("default-permit") := permit                  
              ),
              
              // For viewing invoices: only members of the sales department can view invoices.
              Policy("viewing") := when (action.id === "view") apply DenyOverrides to (
                  Rule("only-sales-department") := deny iff (! (subject.department === "sales")),
                  Rule("default-permit") := permit
              )
          ),
          
          // For banking notes
          Policy("banking-notes") := when (resource.type_ === "banking_note_status") apply DenyOverrides to (
              
              // For viewing banking notes: Only and every member of the ICT department responsible for banking notes can view the status of a sent banking note.
              Policy("viewing") := when (action.id === "view") apply PermitOverrides to (
                  Rule("permit") := permit iff ((subject.department === "ict") & ("banking_notes" in subject.responsible_for)),
                  Rule("default-deny") := deny
              )
          ),
          
          // For paychecks
          Policy("paychecks") := when (resource.type_ === "paycheck") apply DenyOverrides to (
              
              // Only employees which are responsible for payrolling can send paychecks and only to members of their department and only between the 20th and 25th of each month.
              Policy("sending") := when (action.id === "send") apply DenyOverrides to (
                  
                  // First reason to deny: not responsible.
                  Rule("not-responsible") := deny iff (! ("payrolling" in subject.responsible_for)),
                  
                  // Second reason to deny: to other department.
                  Rule("other-department") := deny iff (! (resource.destination_department === subject.department)),
                  
                  // Third reason to deny: wrong date. Simplification: avoid the date operations by using the "date-ok" attribute.
                  Rule("wrong-date") := deny iff (! environment.current_date_between_20_and_25),
                  
                  Rule("default-permit") := permit
              ),
              
              // Only employees which are responsible for payrolling can send paychecks and only to members of their department and only between the 20th and 25th of each month. FIXME description
              Policy("viewing") := when (action.id === "view") apply PermitOverrides to (
                  
                  // First reason to permit: responsible for payrolling.
                  Rule("payrolling") := permit iff ("payrolling" in subject.responsible_for),
                  
                  // Second reason to permit: receivers.
                  Rule("receivers") := permit iff (resource.destination === subject.id),
                  
                  Rule("default-deny") := deny
              )              
          ),
          
          // For sales offers
          Policy("sales-offers") := when (resource.type_ === "sales_offer") apply DenyOverrides to (
              
              // For sending sales offers
              Policy("send") := when (action.id === "send") apply DenyOverrides to (
                  
                  // Only members of the sales department can send sales offers
                  Rule("only-sales-dpt") := deny iff (! (subject.department === "sales")),
                  
                  // Sales offers regarding insurances can only be sent to insurance customers.
                  Rule("insurances") := deny iff (! (resource.destination_customer_type === "insurance")),
                  
                  // Sales offers regarding savings accounts can only be sent to customers who own a savings account.
                  Rule("savings-account") := deny iff (! resource.destination_owns_savings_account),
                  
                  // Sales offers can only be sent between 7am and 7pm.
                  Rule("time") := deny iff (! environment.current_time_between_7_and_19),
                  
                  Rule("default-permit") := permit
              ),
              
              // For sending sales offers to all customers at once
              Policy("send-to-all") := when (action.id === "send_to_all_customers") apply DenyOverrides to (
                  
                  // Only senior members of the sales department can send sales offers.
                  Rule("only-senior") := deny iff (! ((subject.department === "sales") & ("senior" in subject.role))),
                      
                  Rule("default-permit") := permit
              )              
          ),
          
          // For internal communication with local bank offices
          Policy("local-bank-offices") := when (resource.type_ === "bank_office_communication") apply DenyOverrides to (
              
              // For sending to a single bank office
              Policy("send-to-single") := when (action.id === "send") apply DenyOverrides to (
                  
                  // Only bank office managers can send sales offers and only to offices which they are responsible for.
                  Rule("only") := deny iff (! (("bank_office_manager" in subject.role) & (resource.destination_office in subject.assigned_offices))),
                  
                  Rule("default-permit") := permit
              ),
              
              // For sending to all bank offices at once
              Policy("send-to-all") := when (action.id === "send_to_all_offices") apply DenyOverrides to (
                  
                  // Only senior bank office managers can send sales offers.
                  Rule("senior-office-managers") := deny iff (! (("senior" in subject.role) & ("bank_office_manager" in subject.role))),
                  
                  Rule("default-permit") := permit
              )
          ),
          
          // For creating users
          Policy("creating-users") := when ((action.id === "create") & (resource.type_ === "user")) apply DenyOverrides to (
              
              // Only members of the HR department can create users.
              Rule("HR") := deny iff (! (subject.department === "HR")),
              
              // Users can only be created on weekdays between 7am and 7pm. Simplification: avoid the date operations by using the "date-ok" attribute.
              Rule("weekdays") := deny iff (! environment.dateTimeOk),
              
              Rule("default-permit") := permit
          ),
          
          // For creating subtenants
           Policy("creating-subtenants") := when ((action.id === "create") & (resource.type_ === "subtenant")) apply DenyOverrides to (
              
              // Only members of the IT department which are part of senior management can create subtenants.
              Rule("IT") := deny iff (! ((subject.department === "IT") & ("senior" in subject.role))),
                  
              Rule("default-permit") := permit
          ),
          
          // For members of local bank offices
          Policy("local-bank-office") := when ("local-bank-office" in subject.tenant_type) apply DenyOverrides to (
              
              // Only the secretary and the office director of a bank office can read documents sent to the bank office.
              Rule("only") := deny iff (((action.id === "view") & (resource.type_ === "bank_office_communication")) &
                  ! ((("secretary" in subject.role) | ("office_director" in subject.role)) & (resource.owning_tenant === subject.tenant))
              ),
              
              Rule("default-permit") := permit
          )
      )
  )
"""
    
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
      
    val policy =
      Policy("edocs") := apply DenyOverrides to (
          // For members of the provider
          Policy("members-of-provider") := when ("provider" in subject.tenant_name) apply FirstApplicable to (
              
              // No member of the Provider can read a document labeled confidential.
              Rule("confidential") := deny iff ((action.id === "view") & (resource.type_ === "document") & resource.confidential),
              
              // For members of the helpdesk
              Policy("helpdesk") := when ("helpdesk" in subject.role) apply FirstApplicable to (
                  
                  // Members of the helpdesk can view the metadata of every document in the application.
                  Rule("metadata") := permit iff ((action.id === "view") & (resource.type_ === "document_metadata")),
                  
                  // Members of the helpdesk can only view the contents of document belonging to tenants for which they are assigned responsible.
                  Policy("contents") := when ((action.id === "view") & (resource.type_ === "document")) apply PermitOverrides to (
                      Rule("assigned-tenants") := permit iff (resource.owning_tenant in subject.assigned_tenants),
                      Rule("default-deny") := deny
                  ),
                  
                  Rule("default-deny") := deny
              ),
              
              // For application admins
              Policy("admins") := when ("admin" in subject.role) apply FirstApplicable to (
                  
                  // Admins can create new tenants
                  Rule("create-tenant") := permit iff ((action.id === "create") & (resource.type_ === "tenant")),
                  
                  Rule("default-deny") := deny
              )
          ),
          
          // For unregistered receivers
          Policy("unregistered-receivers") := when ("unregistered_receiver" in subject.role) apply DenyOverrides to (
              
              // Unregistered Receivers can only view stuff and can only view documents and document metadata.
              Policy("view document") := apply PermitOverrides to ( 
                  Rule("permit view document") := permit iff ((action.id === "view") & (resource.type_ === "document")),              
                  Rule("default-deny") := deny
              ), 
              
              // A Unregistered Receiver can only view documents sent to him/herself.
              Rule("only sent to him/herself") := deny iff ((action.id === "view") & (resource.destination === subject.id)),
              
              Rule("default-permit") := permit
          ),
          
          // For registered private receivers
          Policy("registered private receivers") := when ("registered_private_receiver" in subject.role) apply PermitOverrides to (
              
              // A Registered Private Receiver can only view documents
              Rule("only view documents") := deny iff ((action.id === "view") & (resource.type_ === "document")),
              
              // The three reasons for a permit: own document, delegated document or delegated all documents.
              Policy("view") := when (action.id === "view") apply PermitOverrides to (
                  
                  // A Registered Private Receiver can view documents which he/she received
                  Rule("own documents") := permit iff (resource.destination === subject.id),
                  
                  // A Registered Private Receiver can view documents which belong to another Registered Receiver which has allowed him/her to read this specific document.
                  Rule("delegated specific") := permit iff (subject.id in resource.delegated_view),
                  
                  // A Registered Private Receiver can view documents belonging to another Registered Private Receiver which has allowed him/her to read all his/her documents.
                  Rule("delegated all") := permit iff (resource.destination in subject.delegated_view_from)
              ),
              
              Rule("default-permit") := permit
          ),
          
          // For tenants in general
          Policy("tenants") := when ("tenant" in subject.tenant_type) apply DenyOverrides to (
              
              // A member of a Tenant can only send a document if the credit of that tenant is sufficient.
              Rule("credit") := deny iff ((action.id === "send") & (resource.type_ === "document") & !subject.tenant_credit_sufficient),
              
              // Tenant isolation
              Rule("tenant isolation") := deny iff (! (resource.owning_tenant === subject.tenant))
          ),
          
          // For specific tenants
          // For Large Bank
          Policy("large-bank") := when ("large-bank" in subject.tenant_name) apply DenyOverrides to (
              
              // For general documents
              Policy("documents") := when (resource.type_ === "document") apply DenyOverrides to (
                  
                  // Every authenticated user can send documents with some restrictions for subtenants and specific types of users.
                  Policy("send") := when (action.id === "send") apply DenyOverrides to (
                      
                      // Members of a subtenant can only send documents to customers of that subtenant.
                      Rule("only to own subtenant") := deny iff (("subtenant" in subject.tenant_type) & !(resource.owning_tenant in subject.customers_of_direct_tenant)),
                      
                      // Members of a bank office can only send documents to external customers whose main office is that bank office.
                      Rule("bank office") := deny iff (("bank_office" in subject.tenant_type) & !(resource.owning_tenant in subject.customers_of_bank_office)),
                      
                      // Refinement of the previous rule: Insurance agents of a bank office can only send documents to insurance customers of that bank office.
                      Rule("insurance agents") := deny iff (("bank_office" in subject.tenant_type) & ("insurance-agent" in subject.role) & !(resource.destination_customer_type === "insurance")),
                      
                      Rule("default-permit") := permit
                  ),
              
                // View documents
                Policy("viewing documents") := when (action.id === "view") apply PermitOverrides to (
                    
                    // A user can read documents sent to himself/herself
                    Rule("only to him/herself") := deny iff (! (resource.destination === subject.id)),
                    
                    // A supervisor can read documents sent by its supervisees.
                    Rule("supervisor") :=  permit iff (("superviser" in subject.role) & (resource.origin in subject.supervisees)),
                    
                    // A project member can read all documents regarding the project.
                    Rule("project members") := permit iff (("project_member" in subject.role) & (resource.project in subject.projects)),
                    
                    // Members of the Large Bank Audit department can read any document sent by any member of Large Bank, except for the paychecks and banking notes, or any other document marked in its meta-data to contain personal information of the customer.
                    Policy("audit") := when ("audit" in subject.role) apply DenyOverrides to (
                        
                        // First reason to deny: not sent by a member of the Large Bank.
                        Rule("not-a-member") := deny iff (! (resource.creating_tenant_name === "large-bank")),
                        
                        // Second reason to deny: paychecks.
                        Rule("paychecks") := deny iff (resource.type_ === "paycheck"), 
                        
                        // Third reason to deny: banking notes.
                        Rule("banking-notes") := deny iff (resource.type_ === "banking_note"),
                        
                        // Fourth reason to deny: marked as containing personal information.
                        Rule("personal-info") := deny iff (resource.contains_personal_information),
                        
                        Rule("default-permit") := permit
                    ),
                    
                    Rule("default-deny") := deny
                )
              ),
              
              // For invoices
              Policy("invoices") := when (resource.type_ === "invoice") apply DenyOverrides to (
                  
                  // For sending invoices: only members of the sales department can send invoices.
                  Policy("sending") := when (action.id === "send") apply DenyOverrides to (
                      Rule("only-sales-department") := deny iff (! (subject.department === "sales")),
                      Rule("default-permit") := permit                  
                  ),
                  
                  // For viewing invoices: only members of the sales department can view invoices.
                  Policy("viewing") := when (action.id === "view") apply DenyOverrides to (
                      Rule("only-sales-department") := deny iff (! (subject.department === "sales")),
                      Rule("default-permit") := permit
                  )
              ),
              
              // For banking notes
              Policy("banking-notes") := when (resource.type_ === "banking_note_status") apply DenyOverrides to (
                  
                  // For viewing banking notes: Only and every member of the ICT department responsible for banking notes can view the status of a sent banking note.
                  Policy("viewing") := when (action.id === "view") apply PermitOverrides to (
                      Rule("permit") := permit iff ((subject.department === "ict") & ("banking_notes" in subject.responsible_for)),
                      Rule("default-deny") := deny
                  )
              ),
              
              // For paychecks
              Policy("paychecks") := when (resource.type_ === "paycheck") apply DenyOverrides to (
                  
                  // Only employees which are responsible for payrolling can send paychecks and only to members of their department and only between the 20th and 25th of each month.
                  Policy("sending") := when (action.id === "send") apply DenyOverrides to (
                      
                      // First reason to deny: not responsible.
                      Rule("not-responsible") := deny iff (! ("payrolling" in subject.responsible_for)),
                      
                      // Second reason to deny: to other department.
                      Rule("other-department") := deny iff (! (resource.destination_department === subject.department)),
                      
                      // Third reason to deny: wrong date. Simplification: avoid the date operations by using the "date-ok" attribute.
                      Rule("wrong-date") := deny iff (! environment.current_date_between_20_and_25),
                      
                      Rule("default-permit") := permit
                  ),
                  
                  // Only employees which are responsible for payrolling can send paychecks and only to members of their department and only between the 20th and 25th of each month. FIXME description
                  Policy("viewing") := when (action.id === "view") apply PermitOverrides to (
                      
                      // First reason to permit: responsible for payrolling.
                      Rule("payrolling") := permit iff ("payrolling" in subject.responsible_for),
                      
                      // Second reason to permit: receivers.
                      Rule("receivers") := permit iff (resource.destination === subject.id),
                      
                      Rule("default-deny") := deny
                  )              
              ),
              
              // For sales offers
              Policy("sales-offers") := when (resource.type_ === "sales_offer") apply DenyOverrides to (
                  
                  // For sending sales offers
                  Policy("send") := when (action.id === "send") apply DenyOverrides to (
                      
                      // Only members of the sales department can send sales offers
                      Rule("only-sales-dpt") := deny iff (! (subject.department === "sales")),
                      
                      // Sales offers regarding insurances can only be sent to insurance customers.
                      Rule("insurances") := deny iff (! (resource.destination_customer_type === "insurance")),
                      
                      // Sales offers regarding savings accounts can only be sent to customers who own a savings account.
                      Rule("savings-account") := deny iff (! resource.destination_owns_savings_account),
                      
                      // Sales offers can only be sent between 7am and 7pm.
                      Rule("time") := deny iff (! environment.current_time_between_7_and_19),
                      
                      Rule("default-permit") := permit
                  ),
                  
                  // For sending sales offers to all customers at once
                  Policy("send-to-all") := when (action.id === "send_to_all_customers") apply DenyOverrides to (
                      
                      // Only senior members of the sales department can send sales offers.
                      Rule("only-senior") := deny iff (! ((subject.department === "sales") & ("senior" in subject.role))),
                          
                      Rule("default-permit") := permit
                  )              
              ),
              
              // For internal communication with local bank offices
              Policy("local-bank-offices") := when (resource.type_ === "bank_office_communication") apply DenyOverrides to (
                  
                  // For sending to a single bank office
                  Policy("send-to-single") := when (action.id === "send") apply DenyOverrides to (
                      
                      // Only bank office managers can send sales offers and only to offices which they are responsible for.
                      Rule("only") := deny iff (! (("bank_office_manager" in subject.role) & (resource.destination_office in subject.assigned_offices))),
                      
                      Rule("default-permit") := permit
                  ),
                  
                  // For sending to all bank offices at once
                  Policy("send-to-all") := when (action.id === "send_to_all_offices") apply DenyOverrides to (
                      
                      // Only senior bank office managers can send sales offers.
                      Rule("senior-office-managers") := deny iff (! (("senior" in subject.role) & ("bank_office_manager" in subject.role))),
                      
                      Rule("default-permit") := permit
                  )
              ),
              
              // For creating users
              Policy("creating-users") := when ((action.id === "create") & (resource.type_ === "user")) apply DenyOverrides to (
                  
                  // Only members of the HR department can create users.
                  Rule("HR") := deny iff (! (subject.department === "HR")),
                  
                  // Users can only be created on weekdays between 7am and 7pm. Simplification: avoid the date operations by using the "date-ok" attribute.
                  Rule("weekdays") := deny iff (! environment.dateTimeOk),
                  
                  Rule("default-permit") := permit
              ),
              
              // For creating subtenants
               Policy("creating-subtenants") := when ((action.id === "create") & (resource.type_ === "subtenant")) apply DenyOverrides to (
                  
                  // Only members of the IT department which are part of senior management can create subtenants.
                  Rule("IT") := deny iff (! ((subject.department === "IT") & ("senior" in subject.role))),
                      
                  Rule("default-permit") := permit
              ),
              
              // For members of local bank offices
              Policy("local-bank-office") := when ("local-bank-office" in subject.tenant_type) apply DenyOverrides to (
                  
                  // Only the secretary and the office director of a bank office can read documents sent to the bank office.
                  Rule("only") := deny iff (((action.id === "view") & (resource.type_ === "bank_office_communication")) &
                      ! ((("secretary" in subject.role) | ("office_director" in subject.role)) & (resource.owning_tenant === subject.tenant))
                  ),
                  
                  Rule("default-permit") := permit
              )
          )
      )
  }
  
}