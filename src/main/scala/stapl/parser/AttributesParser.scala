package stapl.parser

import org.parboiled2._
import stapl.core._
import scala.util.Success
import scala.util.Failure

class AttributesParser(override val input: ParserInput) extends Parser with CommonRules {

  private type AttributeConstructor = (AttributeContainerType, String, AttributeType) => Attribute
  
  def AttributeDefs: Rule1[Seq[Attribute]] = rule { OptWhitespace ~ zeroOrMore(AttributeDef).separatedBy(OptWhitespace) ~ OptWhitespace ~ EOI }
  
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
    capture(str("String") | str("Number") | str("Bool") | str("DateTimeDuration") | str("DayDuration") | 
    str("TimeDuration") | str("DateTime") | str("Day") | str("Time")) ~> {_ match {
      case "Number" => Number
      case "String" => stapl.core.String
      case "Bool" => Bool
      case "DateTime" => DateTime
      case "Day" => Day
      case "Time" => Time
      case "DateTimeDuration" => DateTimeDuration
      case "DayDuration" => DayDuration
      case "TimeDuration" => TimeDuration
    }}
  }
    
  def AttributeName = rule { 
    capture(str("subject") | str("action") | str("resource") | str("environment")) ~> {_ match {
      case "subject" => SUBJECT
      case "action" => ACTION
      case "resource" => RESOURCE
      case "environment" => ENVIRONMENT
    }} ~ 
    '.' ~ capture(Identifier) }
  
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