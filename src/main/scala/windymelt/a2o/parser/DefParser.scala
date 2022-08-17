package windymelt.a2o.parser

import org.parboiled2._

class DefParser(val input: ParserInput) extends Parser {
  def Spaces: Rule0 = rule { anyOf(" \n").* }
  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ Spaces
  }
  def Resources: Rule1[Seq[Syntax.Resource]] = rule {
    Spaces ~ Resource.+ ~ Spaces ~ EOI
  }
  def Resource = rule {
    Spaces ~ ResourceHeader ~ Hash ~ ";" ~> ((name, hash) =>
      Syntax.Resource(name, hash)
    )
  }
  def ResourceHeader = rule {
    ResourceKeyword ~ ResourceName ~ Spaces ~ FatComma
  }
  def Hash: Rule1[Syntax.Hash] = rule {
    "+".? ~ "{" ~ (HashRow * ",") ~ ",".? ~ Spaces ~ "}" ~>
      (hs => Syntax.Hash(hs))
  }
  def HashRow: Rule1[Syntax.HashRow] = rule {
    HashKey ~ Spaces ~ FatComma ~ HashVal ~ Spaces ~> (Syntax.HashRow(_, _))
  }

  def HashKey: Rule1[String] = rule {
    (str("'") ~ capture(AlNumBar) ~ "'") | capture(AlNumBar)
  }
  def HashVal: Rule1[Syntax.HashVal] = rule {
    StringVal | DoubleVal | IntVal | ArrayVal | Hash
  }
  def StringVal = rule {
    (str("'") ~ capture(noneOf("'\n").*) ~ "'" | str("\"") ~ capture(noneOf("\"\"\n").*) ~ "\"" ) ~> (sth => Syntax.StringVal(sth))
  }
  def IntVal =
    rule {
      capture(CharPredicate.Digit.+) ~ Spaces ~> (s =>
        Syntax.IntVal(Integer.parseInt(s))
      )
    }

  def DoubleVal = rule {
    capture(
      CharPredicate.Digit.+ ~ str(".") ~
        CharPredicate.Digit.+
    ) ~ Spaces ~> (s => Syntax.DoubleVal(s.toDouble))
  }

  def ArrayVal: Rule1[Syntax.ArrayVal] = rule {
    "[" ~ (QWList | HashList | StrList) ~ "]" ~> ((sth: Seq[Syntax.HashVal]) =>
      Syntax.ArrayVal(sth)
    )
  }
  def QWList: Rule1[Seq[Syntax.HashVal]] = rule {
    "qw(" ~ (capture(AlNumBar) * " ") ~ Spaces ~ ")" ~> ((ss: Seq[String]) =>
      ss.map(Syntax.StringVal)
    )
  }
  def StrList: Rule1[Seq[Syntax.HashVal]] = rule {
    (StringVal + ",") ~ ",".? ~ Spaces
  }
  def HashList: Rule1[Seq[Syntax.HashVal]] = rule {
    (Hash + ",") ~ ",".? ~ Spaces
  }
  def ResourceKeyword = rule { "resource" }
  def ResourceName = rule { capture(AlNumBar) }
  def FatComma: Rule0 = rule { "=>" }
  def AlNumBar = rule {
    (CharPredicate.Digit | CharPredicate.Alpha | '_' | '$').+
  }
}
