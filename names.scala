import scala.collection.immutable.TreeSet
import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.Map

// To solve http://www.npr.org/2011/05/08/136091948/moms-are-downright-excellent
object names extends App {
  val names = List[String]() ++ Source.fromFile("names").getLines // Name list grabbed from http://names.mongabay.com/female_names.htm
  val sevens = names.filter(_.length == 7)
  val sixes = TreeSet[String]() ++ names.filter(_.length == 6)

  val matches = 
    for (s1 <- sevens;
         s2 <- sevens if (s2 > s1 && s2.startsWith(first4(s1)));
         p <- tails(s1,s2).permutations if sixes.contains(p))
    yield(List(p, s1, s2))
  val rankedMatches = matches.map(m => (m, (m.map(names.indexOf(_)).sum))).sortBy(_._2)
  rankedMatches.foreach(m => printf("%04d: %s from %s,%s\n", m._2, m._1(0), m._1(1), m._1(2)))

  def first4(s: String) = s.substring(0,4)
  def last3(s: String) = s.substring(4,7)
  def tails(a: String, b: String) = last3(a) + last3(b)
}
