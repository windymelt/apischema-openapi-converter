import org.parboiled2._

import windymelt.a2o._

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

val in91 = """{ one => 1,
  two => 2 }
  """
new DefParser(in91).Hash.run()

val in10 = """
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
"""
new DefParser(in10).Resource.run()

val in11 = """
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
};
"""
new DefParser(in11).Resources.run()

val in12 = "[qw( foo )]"
new DefParser(in12).ArrayVal.run()

val in13 = """resource foo => { one => [qw(i have a dream)],};
resource bar => {};"""
new DefParser(in13).Resources.run()

val in14 = "{  }, {  }"
new DefParser(in14).HashList.run()

val in15 = "[ { foo => 1 }, {bar => 3} ]"
new DefParser(in15).ArrayVal.run()
