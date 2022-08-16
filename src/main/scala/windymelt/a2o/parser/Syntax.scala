package windymelt.a2o.parser

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
