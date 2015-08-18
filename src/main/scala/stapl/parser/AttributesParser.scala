package stapl.parser

import org.parboiled2._
import stapl.core._
import scala.util.Success
import scala.util.Failure

/**
 * A parser that parses a sequence of attribute definitions into a `Seq[(String, Attribute)]`.
 * For instance:
 * {{{
 * subject.name = SimpleAttribute("the-name", String)
 * resource.names = ListAttribute(String)
 * }}}
 * will parse to
 * {{{
 * Seq(
 *   ("subject.name", SimpleAttribute(SUBJECT, "the-name", String)),
 *   ("resource.names", ListAttribute(RESOURCE, "names", String))
 * )
 * }}}
 */
class AttributesParser(override val input: ParserInput) extends Parser with CommonRules {

  private type AttributeConstructor = (AttributeContainerType, String, AttributeType) => Attribute
  
  def AttributeDefs: Rule1[Seq[(String, Attribute)]] = rule { OptWhitespace ~ AttributeList ~ OptWhitespace ~ EOI }
  
  def AttributeList = rule { zeroOrMore(AttributeDef).separatedBy(OptWhitespace) }
  
  def AttributeDef = rule { 
    AttributeName ~ OptWhitespace ~ "=" ~ capture("SimpleAttribute" | "ListAttribute") ~> {_.trim match {
      case "SimpleAttribute" => SimpleAttribute(_:AttributeContainerType,_:String,_:AttributeType)
      case "ListAttribute" => ListAttribute(_:AttributeContainerType,_:String,_:AttributeType)
    }} ~ "("  ~ optional(String ~ OptWhitespace ~ ",")~ AttributeType ~ OptWhitespace ~ str(")") ~> {
      (cType: AttributeContainerType, name: String, constr: AttributeConstructor, optName: Option[String], aType: AttributeType) =>
        (cType.toString.toLowerCase + "." + name, constr(cType, optName.getOrElse(name), aType))
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
  
  def parse[Out](attributesString: String)(implicit str: Strategy[Out]): Out = {
    val parser = new AttributesParser(attributesString)
    parser.AttributeDefs.run() match {
      case Success(result) => str.build(result)
      case Failure(e: ParseError) => sys.error(parser.formatError(e))
      case Failure(e) => throw new RuntimeException(e)
    }
  }
  
  
  sealed protected trait Strategy[Out] {
    //type Out
    def build(attrs: Seq[(String, Attribute)]): Out
  }
  
  protected object Strategy {
    implicit val default = Strategies.AttributesOnly
  }
  
  object Strategies {
    
    implicit object AttributesOnly extends Strategy[Seq[Attribute]] {
      //type Out = Seq[Attribute]
      def build(attrs: Seq[(String, Attribute)]): Seq[Attribute] = attrs map {_._2}
    }
    
    implicit object NameToAttribute extends Strategy[Map[String, Attribute]] {
      //type Out = Map[String, Attribute]
      def build(attrs: Seq[(String, Attribute)]): Map[String, Attribute] = attrs.toMap
    }
  }
  
}