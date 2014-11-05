package stapl.parser

import stapl.core._
import org.parboiled2._
import scala.util.Success
import scala.util.Failure

/**
 * A parser that parses a complete STAPL policy with additional attribute definitions at the top of the policy.
 */
class CompleteParser(override val input: ParserInput, attributes: Seq[Attribute]) extends Parser with CommonRules {

  def this(
      input: ParserInput, 
      s: SubjectAttributeContainer, 
      a: ActionAttributeContainer,
      r: ResourceAttributeContainer,
      e: EnvironmentAttributeContainer) = 
        this(input, s.allAttributes ++ a.allAttributes ++ r.allAttributes ++ e.allAttributes)
        
  
  def CompletePolicy: Rule1[AbstractPolicy] = rule { 
    OptWhitespace ~ runSubParser(new AttributesParser(_).AttributeList) ~> {
      (parsedAttributes: Seq[Attribute]) =>
      OptWhitespace ~ runSubParser(new PolicyParser(_, attributes ++ parsedAttributes).AbstractPolicy) ~ OptWhitespace ~ EOI
    } 
  }
  
}

object CompleteParser {
  
  def parse(policyString: String, attributes: Seq[Attribute]): AbstractPolicy = {
    val parser = new CompleteParser(policyString, attributes)
    parser.CompletePolicy.run() match {
      case Success(result) => result
      case Failure(e: ParseError) => sys.error(parser.formatError(e))
      case Failure(e) => throw new RuntimeException(e)
    }
  }
  
  def parse(policyString: String, 
      s: SubjectAttributeContainer, 
      a: ActionAttributeContainer,
      r: ResourceAttributeContainer,
      e: EnvironmentAttributeContainer): AbstractPolicy = {
    val parser = new CompleteParser(policyString, s, a, r, e)
    parser.CompletePolicy.run() match {
      case Success(result) => result
      case Failure(e: ParseError) => sys.error(parser.formatError(e))
      case Failure(e) => throw new RuntimeException(e)
    }
  }
  
}