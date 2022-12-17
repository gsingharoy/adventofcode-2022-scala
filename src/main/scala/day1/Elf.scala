package day1

// Basic model to define an elf
case class Elf(private val calories: List[Int]) {

  // Total calories an elf will carry
  val totalCalories: Int = calories.sum
}

object Elf {
  implicit  def ordering[E <: Elf]: Ordering[E] = new Ordering[E] {
    override def compare(x: E, y: E): Int = x.totalCalories - y.totalCalories
  }
}
