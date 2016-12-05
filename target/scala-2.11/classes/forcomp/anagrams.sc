val abba = List(('a', 2), ('b', 2))

val list = (for ((a, l) <- abba; t <- 1 to l) yield List((a, t))).flatten

val c1 = list.combinations(1).toList

val c2 = list.combinations(2).toList

val combinations = (for (t <- 1 to list.length) yield list.combinations(t).toList).toList

val combinations2 = (List() :: (for (t <- 1 to abba.length) yield list.combinations(t).toList.filter(x => x.length == 1 || !x.forall(y => y._1 == x.head._1))).toList.flatten)

val l = combinations2.length

val ab = List(('a', 2))

def subtract(x: List[(Char, Int)], y: List[(Char, Int)]): List[(Char, Int)] = {
    def substract(occ: (Char, Int), y: List[(Char, Int)]): (Char, Int)= {
        val s = y.filter(x => x._1 == occ._1) match {
            case Nil => 0
            case x :: xs => x._2
        }
        (occ._1, occ._2 - s)
    }

    x.map(o => substract(o, y)).filter(o => o._2 > 0)
}

val subs = subtract(abba, ab)

