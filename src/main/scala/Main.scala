package windymelt.a2o

import org.parboiled2._
import org.parboiled2.support.hlist.HNil

object Syntax {
  sealed trait HashVal
  case class StringVal(s: String) extends HashVal
  case class IntVal(i: Int) extends HashVal
  case class DoubleVal(d: Double) extends HashVal
  case class ArrayVal(arr: Seq[HashVal]) extends HashVal
  case class Hash(hs: Seq[HashRow]) extends HashVal
  case class HashRow(key: String, value: HashVal)
  case class Resource(name: String, hash: Hash)
}

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
    str("'") ~ capture(noneOf("'\n").*) ~ "'" ~> (sth => Syntax.StringVal(sth))
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
    "[" ~ (QWList | StrList | HashList) ~ "]" ~> ((sth: Seq[Syntax.HashVal]) =>
      Syntax.ArrayVal(sth)
    )
  }
  def QWList: Rule1[Seq[Syntax.HashVal]] = rule {
    "qw(" ~ (capture(AlNumBar) * " ") ~ Spaces ~ ")" ~> ((ss: Seq[String]) =>
      ss.map(Syntax.StringVal)
    )
  }
  def StrList: Rule1[Seq[Syntax.HashVal]] = rule {
    (StringVal * ",") ~ ",".? ~ Spaces
  }
  def HashList: Rule1[Seq[Syntax.HashVal]] = rule {
    (Hash * ",") ~ ",".? ~ Spaces
  }
  def ResourceKeyword = rule { "resource" }
  def ResourceName = rule { capture(AlNumBar) }
  def FatComma: Rule0 = rule { "=>" }
  def AlNumBar = rule {
    (CharPredicate.Digit | CharPredicate.Alpha | '_' | '$').+
  }
}

object Main extends App {
  def sampleSchema: String = """
  resource figure => {
    type => 'object',
    description => 'Figure, which includes weight and height',
    properties => {
        weight  => {
            type => 'number',
            description => 'Weight(kg)',
            example => 50,
        },
        height  => {
            type => 'number',
            description => 'Height(m)',
            example => 1.6,
        },
    },
    required => ['weight', 'height'],
};

resource bmi => {
    type => 'object',
    description => 'Body mass index',
    properties => {
        value  => {
            type => 'number',
            description => 'bmi value',
            example => 19.5,
        },
    },
    required => ['value'],
};
  """

  println("Hello world!")
  println(new DefParser(sampleSchema).Resources.run())
}
