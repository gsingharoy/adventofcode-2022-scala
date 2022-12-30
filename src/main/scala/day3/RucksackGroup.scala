package day3

case class RucksackGroup(rucksack1: Rucksack, rucksack2: Rucksack, rucksack3: Rucksack)
    extends Group {

  lazy val duplicatedItems: List[Char] =
    listMatcher(rucksack1.allItems, List(rucksack2.allItems, rucksack3.allItems))

  lazy val badge: Option[Char] = duplicatedItems
    .sortWith(RucksackUtils.itemScore(_) > RucksackUtils.itemScore(_))
    .headOption

}

object RucksackGroup {

  def constructFromRucksacks(rucksacks: List[Rucksack]): List[RucksackGroup] = rucksacks
    .grouped(3)
    .toList
    .flatMap((rList) =>
      rList match {
        case _ if rList.length != 3         => None
        case head1 :: head2 :: head3 :: Nil => Some(RucksackGroup(head1, head2, head3))
        case _                              => None
      }
    )
}
