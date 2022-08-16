package windymelt.a2o

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
  println(new parser.DefParser(sampleSchema).Resources.run())
}
