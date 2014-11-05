package stapl.parser

import org.parboiled2._

/**
 * A trait containing a number of parser rules that are common to multiple parsers.
 */
trait CommonRules { self: Parser =>

  def Identifier = rule { IdentifierStartChar ~ optional(zeroOrMore(IdentifierChar)) }
  
  def String = rule {"\"" ~ capture(zeroOrMore(!"\"" ~ Char)) ~ "\""} // FIXME not correct !!! \" can be in string
  
  def OptWhitespace = rule { optional(Whitespace) }
  
  def Whitespace = rule { 
    oneOrMore(
        oneOrMore(WhitespaceChar) 
        | "/*" ~ zeroOrMore(!"*/" ~ Char) ~ "*/"
        | "//" ~ zeroOrMore(" " | "\t") ~ zeroOrMore(!anyOf("\r\n") ~ Char) ~ ("\r\n"| '\r' | '\n')
    )
  }
  
  val WhitespaceChar = CharPredicate(" \n\r\t\f")
  
  val IdentifierStartChar = CharPredicate.Alpha ++ "_$"
  
  val IdentifierChar = IdentifierStartChar ++ CharPredicate.Digit
  
  val Char = CharPredicate.All
  
  val Digit = CharPredicate.Digit
  
  val Digit19 = CharPredicate.Digit19
  
}