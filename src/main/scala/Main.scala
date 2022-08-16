package windymelt.a2o

import org.parboiled2._

object Syntax {
  sealed trait HashVal
  case class StringVal(s: String) extends HashVal
  case class IntVal(i: Int) extends HashVal
  case class ArrayVal(arr: Seq[StringVal]) extends HashVal
  case class HashRow(key: String , value: HashVal)
}

class DefParser(val input: ParserInput) extends Parser {
  def Spaces = rule { zeroOrMore(' ') }
  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ Spaces
  }
  def ResourceHeader = rule { ResourceKeyword ~ ResourceName ~ Spaces ~ FatComma }
  def Hash = rule { optional("+") ~ "{" ~ capture(HashRow * ",") ~> (sth => ???) ~ "}" }
  def HashRow: Rule1[Syntax.HashRow] = rule { HashKey ~ Spaces ~ FatComma ~ HashVal ~ Spaces } ~> ( (k, v) => Syntax.HashRow(k, v) )
  def HashKey = rule { (str("'") ~ capture(AlNumBar) ~ "'") | capture(AlNumBar) }
  def HashVal = rule { StringVal | IntVal | ArrayVal }
  def StringVal = rule { str("'") ~ capture(oneOrMore(noneOf("'"))) ~ "'" } ~> Syntax.StringVal.apply
  def IntVal = rule { capture(oneOrMore(CharPredicate.Digit)) } ~> (s => s.par)
  def ArrayVal = rule { "[" ~ QWList ~ "]" }
  def QWList = rule { "qw(" ~ zeroOrMore(capture(AlNumBar) ~ zeroOrMore(' ')) ~ ")" }
  def ResourceKeyword = rule {"resource"}
  def ResourceName = rule { capture(AlNumBar) }
  def FatComma = rule {"=>"}
  def AlNumBar = rule { oneOrMore(CharPredicate.Digit | CharPredicate.Alpha | '_' | '$') }
}

@main def hello: Unit = 
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"
