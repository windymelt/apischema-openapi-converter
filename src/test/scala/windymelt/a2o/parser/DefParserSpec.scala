package windymelt.a2o.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Success

class DefParserSpec extends AnyFlatSpec with Matchers {
  "AlNumBar" should "be parsed as alphabet + number + some symbols" in {
    val x = "foobar2000"
    new DefParser(x).AlNumBar.run() shouldBe Success(())
  }

  "FatComma" should "match =>" in {
    val x = "=>"
    new DefParser(x).FatComma.run() shouldBe Success(())
  }

  "ArrayVal" should "match ['foo', 'bar']" in {
    val strings = "['foo', 'bar']"

    new DefParser(strings).ArrayVal.run() shouldBe Success(Syntax.ArrayVal(Seq(Syntax.StringVal("foo"), Syntax.StringVal("bar"))))
  }
  it should "match [qw(foo bar)]" in {
    val qwstrings = "[qw(foo bar)]"
    new DefParser(qwstrings).ArrayVal.run() shouldBe Success(Syntax.ArrayVal(Seq(Syntax.StringVal("foo"), Syntax.StringVal("bar"))))
  }
  it should "match [{foo => 1}, {bar => 2}]" in {
    val hashlist = "[{ foo => 1}, {bar => 2}]"
    new DefParser(hashlist).ArrayVal.run() shouldBe Success(
      Syntax.ArrayVal(Seq(
        Syntax.Hash(
          Seq(Syntax.HashRow("foo", Syntax.IntVal(1))),
        ),
        Syntax.Hash(
          Seq(Syntax.HashRow("bar", Syntax.IntVal(2))),
        )
      ))
    )
  }

  "DoubleVal" should "match fundamental double" in {
    val x = "3.1415"
    new DefParser(x).DoubleVal.run() shouldBe Success(Syntax.DoubleVal(3.1415))
  }

  "Intval" should "match fundamental int" in {
    val x = "42"
    new DefParser(x).IntVal.run() shouldBe Success(Syntax.IntVal(42))
  }

  "StringVal" should "match string" in {
    val x = "\'余部橋梁（あまるべきょうりょう）は、兵庫県美方郡香美町香住区（旧・城崎郡香住町）余部、西日本旅客鉄道（JR西日本）山陰本線鎧駅 - 餘部駅間にある橋梁（単線鉄道橋）である。\'"
    new DefParser(x).StringVal.run() shouldBe Success(Syntax.StringVal("余部橋梁（あまるべきょうりょう）は、兵庫県美方郡香美町香住区（旧・城崎郡香住町）余部、西日本旅客鉄道（JR西日本）山陰本線鎧駅 - 餘部駅間にある橋梁（単線鉄道橋）である。"))
  }

  "HashVal" should "match hash" in {
    val h = """{
  'windy' => 1,
  melt => 2
}
"""
    new DefParser(h).Hash.run() shouldBe Success(
      Syntax.Hash(Seq(
        Syntax.HashRow("windy", Syntax.IntVal(1)),
        Syntax.HashRow("melt", Syntax.IntVal(2)),
      ))
    )
  }

  "EndpointMethod" should "match method names" in {
    val get = "GET"
    val post = "POST"
    val put = "PUT"
    val delete = "DELETE"

    Seq(get, post, put, delete) foreach { m =>
      (new DefParser(m).EndpointMethod.run()) shouldBe Success(m)
    }
  }

  "Endpoint" should "match endpoint notation" in {
    val x = """POST '/bmi' => {
    title           => 'BMI API',
};"""
    new DefParser(x).Endpoint.run() shouldBe Success(
      Syntax.Endpoint("POST", Syntax.StringVal("/bmi"), Syntax.Hash(
        Seq(Syntax.HashRow("title", Syntax.StringVal("BMI API")))
      ))
    )
  }
}
