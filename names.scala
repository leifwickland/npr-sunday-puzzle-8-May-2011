// To solve http://www.npr.org/2011/05/08/136091948/moms-are-downright-excellent
object names extends App {
  val names = List[String]() ++ scala.io.Source.fromFile("names").getLines // Name list grabbed from http://names.mongabay.com/female_names.htm
  val sevens = names.filter(_.length == 7).sorted
  val sixes = Set[String]() ++ names.filter(_.length == 6)

  val matches = 
    for ((s1, i) <- sevens.zipWithIndex;
         s2 <- sevens.view(i + 1, sevens.indexWhere(!_.startsWith(s1.substring(0, 4)), i + 1));
         p <- (s1.substring(4, 7) + s2.substring(4, 7)).permutations if sixes.contains(p))
    yield(List(p, s1, s2))
  val rankedMatches = matches.map(m => (m, (m.map(names.indexOf(_)).sum))).sortBy(_._2)
  rankedMatches.foreach(m => printf("%04d: %s from %s,%s\n", m._2, m._1(0), m._1(1), m._1(2)))
}
