package stapl.parser

import org.parboiled2._
import stapl.core._
import scala.util.Success
import scala.util.Failure

/**
 * A parser that parses a STAPL policy without any attribute definitions provided in the policy.
 */
class PolicyParser(override val input: ParserInput, attributes: Seq[Attribute]) extends Parser with CommonRules {
  
  def this(
      input: ParserInput, 
      s: SubjectAttributeContainer, 
      a: ActionAttributeContainer,
      r: ResourceAttributeContainer,
      e: EnvironmentAttributeContainer) = 
        this(input, s.allAttributes ++ a.allAttributes ++ r.allAttributes ++ e.allAttributes)
  
  val attributeMap: Map[String, Attribute] = Map((for(attribute <- attributes) yield {
    val (cType, name) = (attribute.cType, attribute.name)
    val key = s"${cType.toString.toLowerCase}.$name"
    (key, attribute)
  }):_*)
  
  
  def Stapl = rule { OptWhitespace ~ AbstractPolicy ~ OptWhitespace ~ EOI }
  
  def AbstractPolicy: Rule1[AbstractPolicy] = rule { Policy | Rule }
  
  def Policy = rule { 
    "Policy(" ~ String ~ ")" ~ ":=" ~ optional("when" ~ "(" ~ Expression ~ OptWhitespace ~ ")") ~
    str("apply") ~ Whitespace ~ CombinationAlg ~ Whitespace ~ "to" ~ "(" ~
    oneOrMore(AbstractPolicy).separatedBy(OptWhitespace ~ ",") ~ OptWhitespace ~ str(")") ~> {
      (id: String, targOpt: Option[Expression], alg: CombinationAlgorithm, subp: Seq[AbstractPolicy]) => 
        targOpt map {targ => new Policy(id)(target=targ, pca=alg, subpolicies=subp.toList) } getOrElse new Policy(id)(pca=alg, subpolicies=subp.toList)
    }
  }

  def Rule = rule { "Rule(" ~ String ~ ")" ~ ":=" ~ Effect ~ optional(Whitespace ~ IffExpression) ~> {
    (id: String, e: Effect, expOpt: Option[Expression]) => 
      expOpt map {exp => new stapl.core.Rule(id)(effect=e, condition=exp) } getOrElse new stapl.core.Rule(id)(effect=e)
  } }
  
  def Effect = rule { str("permit") ~> {() => Permit} | str("deny") ~> {() => Deny} }
  
  def IffExpression = rule { "iff" ~ "(" ~ Expression ~ OptWhitespace ~ str(")") }
  
  
  def Expression: Rule1[Expression] = rule { 
    BinaryExpression | (ExpressionTerm ~ zeroOrMore(OptWhitespace ~ "|" ~ ExpressionTerm ~> Or ))
  }
  
  def ExpressionTerm: Rule1[Expression] = rule { ParensExpression ~ zeroOrMore(OptWhitespace ~ "&" ~ ParensExpression ~> And) }
  
  def ParensExpression: Rule1[Expression] = rule { ("(" ~ Expression ~ OptWhitespace ~ str(")")) | UnaryExpression }
  
  def BinaryExpression = rule { (Value ~ OptWhitespace ~ capture("===" | "in" | "lteq" | "gteq" | "gt" | "lt") ~ Value) ~> {
    (left: Value, op: String, right: Value) => op.trim match {
      case "===" => left === right
      case "in" => left in right
      case "lteq" => left lteq right
      case "gteq" => left gteq right
      case "gt" => left gt right
      case "lt" => left lt right
    }
  } }
  
  def UnaryExpression = rule { 
    ("!" ~ ParensExpression) ~> Not | 
    Attribute ~> {boolAttributeToExpression(_)} | 
    str("true") ~> {() => boolean2Expression(true)} | 
    str("false") ~> {() => boolean2Expression(false)} | 
    str("AlwaysTrue") ~> {() => AlwaysTrue} | 
    str("AlwaysFalse") ~> {() => AlwaysFalse}
  }
  
  def Value: Rule1[Value] = rule { (ParensOperation | Attribute | ConcreteValue) }
  
  def ParensOperation: Rule1[Operation] = rule { "(" ~ BinaryOperation ~ OptWhitespace ~ str(")") | UnaryOperation }
  
  def Operation: Rule1[Operation] = rule { BinaryOperation | UnaryOperation }
  
  def BinaryOperation = rule { OperationTerm ~ zeroOrMore(OptWhitespace ~ (
        "+" ~ OperationTerm ~> Addition
      | "-" ~ OperationTerm ~> Subtraction
  )) }
  
  def UnaryOperation = rule { "abs" ~ "(" ~ UnaryOpFactor ~ OptWhitespace ~ str(")") ~> AbsoluteValue }
  
  def UnaryOpFactor = rule { Operation | Attribute | ConcreteValue }
  
  def OperationTerm: Rule1[Value] = rule { OperationFactor ~ zeroOrMore(OptWhitespace ~ (
        "*" ~ OperationFactor ~> Multiplication
      | "/" ~ OperationFactor ~> Division
  )) }
  
  def OperationFactor = rule { Value }
  
  def ConcreteValue = rule { 
    BoolValue | StringValue | DayValue | TimeValue | 
    DateTimeValue| DayDurationValue | TimeDurationValue | NumberValue
  } // TODO add sequence literals
  
  def NumberValue = rule { Double ~> {double2Value(_)} | Long ~> {long2Value(_)} }
  
  def BoolValue = rule { capture(str("true") | str("false")) ~> {(b:String) => boolean2Value(b.toBoolean)} }
  
  def StringValue = rule { String ~> {string2Value(_)} }
  
  def DayValue = rule { "Day" ~ "(" ~ 3.times(Integer).separatedBy(OptWhitespace ~ ",") ~ str(")") ~> {
    (nrs: Seq[Long]) => 
      val Seq(year, month, day) = nrs map {_.toInt}
      Day(year, month, day)
  } }
  
  def TimeValue = rule { TimeWithMillis | TimeWithoutMillis }
  
  def TimeWithMillis = rule { "Time" ~ "(" ~ 4.times(Integer).separatedBy(OptWhitespace ~ ",") ~ str(")") ~> {
    (nrs: Seq[Long]) => 
      val Seq(hour, min, sec, mil) = nrs map {_.toInt}
      Time(hour, min, sec, mil)
  } }
  
  def TimeWithoutMillis = rule { "Time" ~ "(" ~ 3.times(Integer).separatedBy(OptWhitespace ~ ",") ~ str(")") ~> {
    (nrs: Seq[Long]) => 
      val Seq(hour, min, sec) = nrs map {_.toInt}
      Time(hour, min, sec)
  } }
  
  def DateTimeValue = rule { DateTimeWithMillis | DateTimeWithoutMillis }
  
  def DateTimeWithMillis = rule { "DateTime" ~ "(" ~ 7.times(Integer).separatedBy(OptWhitespace ~ ",") ~ str(")") ~> {
    (nrs: Seq[Long]) => 
      val Seq(year, month, day, hour, min, sec, mil) = nrs map {_.toInt}
      DateTime(year, month, day, hour, min, sec, mil)
  } }
  
  def DateTimeWithoutMillis = rule { "DateTime" ~ "(" ~ 6.times(Integer).separatedBy(OptWhitespace ~ ",") ~ str(")") ~> {
    (nrs: Seq[Long]) => 
      val Seq(year, month, day, hour, min, sec) = nrs map {_.toInt}
      DateTime(year, month, day, hour, min, sec)
  } }
  
  def DayDurationValue = rule { Integer ~ '.' ~ capture(str("days") | str("months") | str("years")) ~> {
    (nr: Long, unit: String) => unit match {
      case "days" => Days(nr.toInt)
      case "months" => Months(nr.toInt)
      case "years" => Years(nr.toInt)
    }
  } }
  
  def TimeDurationValue = rule { Integer ~ '.' ~ capture(str("hours") | str("minutes") | str("seconds") | str("millis")) ~> {
    (nr: Long, unit: String) => unit match {
      case "hours" => Hours(nr.toInt)
      case "minutes" => Minutes(nr.toInt)
      case "seconds" => Seconds(nr.toInt)
      case "millis" => Millis(nr.toInt)
    }
  } }
  
  //def DateTimeDurationValue = ???
  
  def Attribute = rule { capture((str("subject") | str("action") | str("resource") | str("environment")) ~ '.' ~ Identifier) ~> {attributeMap(_)} }
  
  
  def CombinationAlg = rule { Map("PermitOverrides" -> PermitOverrides, "DenyOverrides" -> DenyOverrides, "FirstApplicable" -> FirstApplicable) }
  
  def Double = rule { 
    capture(optional('-') ~ (Digit19 ~ oneOrMore(Digit) | Digit) ~ '.' ~ oneOrMore(Digit)) ~> {_.toDouble} ~ optional(ch('f') | ch('F') | ch('d') | ch('D')) 
  }
  
  def Integer = rule { capture(optional('-') ~ (Digit19 ~ oneOrMore(Digit) | Digit)) ~> {_.toLong} ~ optional(ch('L') | ch('l')) }
  
  def Long = rule { Integer ~ optional(ch('L') | ch('l')) }
  
  
  /**
   * Automatically add (optional) whitespace at the end of strings.
   * Use `str("string")` to override this behavior.
   */
  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ OptWhitespace
  }
  
}

object PolicyParser {
  
  def parse(policyString: String, attributes: Seq[Attribute]): AbstractPolicy = {
    val parser = new PolicyParser(policyString, attributes)
    parser.Stapl.run() match {
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
    val parser = new PolicyParser(policyString, s, a, r, e)
    parser.Stapl.run() match {
      case Success(result) => result
      case Failure(e: ParseError) => sys.error(parser.formatError(e))
      case Failure(e) => throw new RuntimeException(e)
    }
  }
  
}