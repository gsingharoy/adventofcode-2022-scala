package day1

// Basic model to define an elf
case class Elf(private val calories: List[Int]) {

  // Total calories an elf will carry
  val totalCalories: Int = calories.sum
}
