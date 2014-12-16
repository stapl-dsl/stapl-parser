package stapl.parser

import org.parboiled2._
import stapl.core._
import scala.util.Success
import scala.util.Failure

/**
 * A parser that parses a sequence of attribute difinitions into a `Seq[Attribute]`
 */
class AttributesParser(override val input: ParserInput) extends Parser with CommonRules {

  private type AttributeConstructor = (AttributeContainerType, String, AttributeType) => Attribute
  
  def AttributeDefs: Rule1[Seq[Attribute]] = rule { OptWhitespace ~ AttributeList ~ OptWhitespace ~ EOI }
  
  def AttributeList = rule { zeroOrMore(AttributeDef).separatedBy(OptWhitespace) }
  
  def AttributeDef = rule { 
    AttributeName ~ OptWhitespace ~ "=" ~ capture("SimpleAttribute" | "ListAttribute") ~> {_.trim match {
      case "SimpleAttribute" => SimpleAttribute(_:AttributeContainerType,_:String,_:AttributeType)
      case "ListAttribute" => ListAttribute(_:AttributeContainerType,_:String,_:AttributeType)
    }} ~ "(" ~ AttributeType ~ OptWhitespace ~ str(")") ~> {
      (cType: AttributeContainerType, name: String, constr: AttributeConstructor, aType: AttributeType) =>
        constr(cType, name, aType)
    }
  }
  
  def AttributeType = rule {
    str("String") ~ push(stapl.core.String) | 
    str("Number") ~ push(Number) |
    str("Bool") ~ push(Bool) |
    str("DateTimeDuration") ~ push(DateTimeDuration) |
    str("DayDuration") ~ push(DayDuration) | 
    str("TimeDuration") ~ push(TimeDuration) |
    str("DateTime") ~ push(DateTime) |
    str("Day") ~ push(Day) |
    str("Time") ~ push(Time)
  }
    
  def AttributeName = rule { 
    (
      str("subject") ~ push(SUBJECT) | 
      str("action") ~ push(ACTION) | 
      str("resource") ~ push(RESOURCE) | 
      str("environment") ~ push(ENVIRONMENT)
    ) ~ '.' ~ capture(Identifier) 
  }
  
  /**
   * Automatically add (optional) whitespace at the end of strings.
   * Use `str("string")` to override this behavior.
   */
  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ OptWhitespace
  }
  
}

object AttributesParser {
  
  def parse(attributesString: String): Seq[Attribute] = {
    val parser = new AttributesParser(attributesString)
    parser.AttributeDefs.run() match {
      case Success(result) => result
      case Failure(e: ParseError) => sys.error(parser.formatError(e))
      case Failure(e) => throw new RuntimeException(e)
    }
  }
  
}

private object TestAttributeParser extends App {
  
  val parser = new AttributesParser(
"""
subject.name = SimpleAttribute(String)
resource.names = ListAttribute(String)
environment.nrnames = SimpleAttribute (  Number )
action.day = SimpleAttribute(DayDuration)
""")
  parser.AttributeDefs.run() match {
    case Success(result) => println(result)
    case Failure(e: ParseError) => print(parser.formatError(e, showTraces=true))
    case Failure(e) => e.printStackTrace()
  }
  
}