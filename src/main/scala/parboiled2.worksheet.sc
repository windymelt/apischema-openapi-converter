import org.parboiled2._

import windymelt.a20._

val in1 = "foo_bar"
new DefParser(in1).ResourceName.run()

val in2 = "resource"
new DefParser(in2).ResourceKeyword.run()

val in3 = "resource foo_bar =>"
new DefParser(in3).ResourceHeader.run()

val in4 = "'foobar にゃんにゃん'"
new DefParser(in4).StringVal.run()

val in5 = "12345"
new DefParser(in5).IntVal.run()

val in6 = "qw(foo bar buzz)"
new DefParser(in6).QWList.run()

val in7 = "foo_bar => [ qw(windy melt) ]"
new DefParser(in7).HashRow.run()

val in8 = "+{ foo_bar => 12345 }"
new DefParser(in8).Hash.run()

val in9 = "{ one => 1, two => 2 }"
new DefParser(in9).Hash.run()
