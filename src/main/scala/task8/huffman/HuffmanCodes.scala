package task8.huffman

import scala.annotation.tailrec

object Run extends App {
  val text = "Practical analysis of advanced algorithms".toCharArray.toList

  println(text.mkString(""))

  import HuffmanCodes._

  val tree = createCodeTree(text)

  println("tree generated with weight: " + weight(tree))

  val encoded = encode(tree)(text)
  println("encoding: " + encoded.mkString(""))

  val decoded = decode(tree, encoded)
  println("decoding: " + decoded.mkString(""))
}

object HuffmanCodes {

  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, weight) => weight
    case Leaf(_, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, chars, _) => chars
    case Leaf(char, _) => List(char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree): Fork =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees


  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] =
    chars.groupBy(identity).mapValues(_.map(_ => 1).sum).toList

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    freqs.sortBy(_._2).map(pair => Leaf(pair._1, pair._2))


  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case List() => false
    case _ :: xs => xs.isEmpty
  }


  def combine(trees: List[CodeTree]): List[CodeTree] = {
    @tailrec
    def insert(y: CodeTree, ys: List[CodeTree]): List[CodeTree] = ys match {
      case List() => List(y)
      case x :: xs => if (weight(y) <= weight(x)) y :: ys else insert(y, xs)
    }

    trees match {
      case List() => List()
      case head :: List() => List(head)
      case first :: second :: tail => insert(makeCodeTree(first, second), tail)
    }
  }


  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])
           (trees: List[CodeTree]): List[CodeTree] = trees match {
    case result if done(result) => result
    case _ =>
      val next = merge(trees)
      until(done, merge)(next)
  }


  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head


  // Part 3: Decoding

  type Bit = Int


  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    @tailrec
    def recFunc(current: CodeTree, bitsLeft: List[Bit], acum: List[Char]): List[Char] = (bitsLeft, current) match {
      case (List(), Leaf(char, _)) =>
        acum ::: List(char)
      case (_, Leaf(char, _)) =>
        recFunc(tree, bitsLeft, acum ::: List(char))
      case (x :: xs, Fork(left, _, _, _)) if x == 0 =>
        recFunc(left, xs, acum)
      case (x :: xs, Fork(_, right, _, _)) if x == 1 =>
        recFunc(right, xs, acum)
    }

    recFunc(tree, bits, List())
  }


  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)


  def decodedSecret: List[Char] =
    decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    @tailrec
    def recFunc(current: CodeTree, textLeft: List[Char], acum: List[Bit]): List[Bit] = (textLeft, current) match {
      case (List(), _) =>
        acum
      case (_, Leaf(_, _)) =>
        recFunc(tree, textLeft.tail, acum)
      case (_, Fork(left, _, _, _)) if HuffmanCodes.chars(left).contains(textLeft.head) =>
        recFunc(left, textLeft, acum ::: List(0))
      case (_, Fork(_, right, _, _)) if HuffmanCodes.chars(right).contains(textLeft.head) =>
        recFunc(right, textLeft, acum ::: List(1))
    }

    recFunc(tree, text, List())
  }

  // Part 4b: Encoding using code table

  type CodeTable = Map[Char, List[Bit]]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = table(char)

  def convert(tree: CodeTree): CodeTable = {
    def func(current: CodeTree, acum: List[Bit]): CodeTable = current match {
      case Fork(left, right, chars, weight) => func(left, acum ::: List(0)) ++ func(right, acum ::: List(1))
      case Leaf(char, weight) => Map(char -> acum)
    }

    func(tree, List())
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = ???

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val table = convert(tree)

    def function(text: List[Char]): List[Bit] = text match {
      case Nil => List()
      case char :: textLeft => codeBits(table)(char) ::: function(textLeft)
    }

    function(text)
  }
}
