package com.ikempf

import cats.implicits._
import com.ikempf.BirthdayParadox.showParadox

import scala.collection.immutable
import scala.collection.immutable.Seq
import scala.util.Random

object Main extends App {

  showParadox(23)
  showParadox(50)
  showParadox(75)
  showParadox(100)


}

object BirthdayParadox {
  val Gen = new Random()
  val Iterations = 9999

  def showParadox(n: Int): Unit = {
    val chance = paradox(n)
    println(show"Sample size: $n, matching birthday chance: $chance %")
  }

  def randomBirthday(): Int =
    Gen.nextInt(365) + 1

  def randomBirthdays(n: Int): immutable.IndexedSeq[Int] =
    Range.inclusive(1, n).map(_ => randomBirthday())

  def paradox(n: Int): Double = {
    val matchingBirthdays =
      Range
        .inclusive(0, Iterations)
        .map(_ => hasMatchingBirthdays(randomBirthdays(n)))
        .count(identity)

    matchingBirthdays.toDouble / Iterations
  }

  def hasMatchingBirthdays(birthdays: Seq[Int]): Boolean =
    birthdays.distinct.length != birthdays.length

}
