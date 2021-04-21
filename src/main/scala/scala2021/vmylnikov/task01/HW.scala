package scala2021.vmylnikov.task01

object HW {

  def main(args: Array[String]): Unit = {
    val counts = Array(
      "900,google.com",
      "60,mail.yahoo.com",
      "10,mobile.sports.yahoo.com",
      "40,sports.yahoo.com",
      "10,stackoverflow.com",
      "2,en.wikipedia.org",
      "1,es.wikipedia.org",
      "1,mobile.sports"
    )

    myFunc(counts).foreach(t => println(t._1 + " " + t._2))
  }

  def myFunc(data: Array[String]): List[(Int, String)] = {
    def getSubDomains(domain: String): List[String] = {
      val parentDomain = if (domain.contains('.')) domain.substring(domain.indexOf('.') + 1) else null
      parentDomain match {
        case null => List()
        case _ => parentDomain :: getSubDomains(parentDomain)
      }
    }

    def toListWithSubDomains(tuple: (Int, String)): List[(Int, String)] = {
      val domain = tuple._2
      val subDomains = getSubDomains(domain)
      subDomains match {
        case List() => List(tuple)
        case _ => tuple :: subDomains.map(domain => (tuple._1, domain))
      }
    }

    data.map(s => {
      val arr = s.split(",")
      (arr(0).toInt, arr(1))
    }).toList
      .flatMap(t => toListWithSubDomains(t))
      .map(_.swap)
      .groupMapReduce(t => t._1)(t => t._2)(_ + _)
      .toList
      .map(_.swap)
      .sortWith((t1, t2) => {
        val domainReverse1 = t1._2.split('.').reverse.mkString(".")
        val domainReverse2 = t2._2.split('.').reverse.mkString(".")
        domainReverse1.compareTo(domainReverse2) < 0
      })
  }

}
