package homework1




/**
  * В этом задании необходимо реализовать функцию BoxPlan.plan, которая возвращает список коробок
  * Также необходимо дописать тест для проверки решения.
  *
  * Существует несколько видов коробок:
  * * PlayStationBox может в себе хранить только PlayStation
  * * GuitarBox только Guitar
  * * EaselBox только Easel
  * * BasicBox может хранить любые вещи, но у каждой коробки есть максимальная вместительность - 4
  * * BigBox также может хранить любые вещи, но у него тоже есть максимальная вместительность
  *
  * Между BasicBox и BigBox лучше выбирать BasicBox.
  */

import scala.collection.mutable.ArrayBuffer

sealed trait Box
case class PlayStationBox() extends Box
case class GuitarBox() extends Box
case class EaselBox() extends Box
case class BasicBox() extends Box {
  val size: Int = 4
}
case class BigBox() extends Box {
  val size: Int = 10
}

trait Stuff
case class PlayStation() extends Stuff
case class Guitar() extends Stuff
case class TV(size: Int) extends Stuff
case class Easel() extends Stuff
case class Book() extends Stuff
case class Cat() extends Stuff
case class Uculele() extends Stuff
case class Dish() extends Stuff
case class Shoes() extends Stuff

object BoxPlan extends App {

  def plan(stuff: Seq[Stuff]): Seq[Box] = {
    val bigBoxSize = 10
    val basicBoxSize = 4
    var boxSeq = Seq[Box]()
    val vacantBoxes = ArrayBuffer[Int]() //массив в котором храняются пустующие места в коробках в порядке увеличения свободного места
    stuff.foreach { x =>
      x match {
        case PlayStation() => boxSeq +:= PlayStationBox()
        case Guitar() => boxSeq +:= GuitarBox()
        case Easel() => boxSeq +:= EaselBox()
        case Book() | Cat() | Uculele() | Dish() | Shoes() =>
          if (vacantBoxes.isEmpty) {
            boxSeq +:= BasicBox()
            vacantBoxes += basicBoxSize - 1
          }
          else {
            if (vacantBoxes(0) == 1) vacantBoxes.remove(0) else vacantBoxes(0) -= 1
          }
        case TV(size) => if (size > bigBoxSize || size <= 0) throw new IllegalArgumentException("Illegal size of TV") else {
          vacantBoxes.find(_ >= size) match {
            case None =>
              if (size > basicBoxSize) {
                boxSeq +:= BigBox()
                vacantBoxes += bigBoxSize - size
              }
              else if (size < basicBoxSize) {
                boxSeq +:= BasicBox()
                vacantBoxes += basicBoxSize - size
              }
              else boxSeq +:= BasicBox()
              if (!vacantBoxes.isEmpty && size != basicBoxSize) vacantBoxes.sortWith(_ < _)
            case Some(num) =>
              vacantBoxes(num) -= size
              if (vacantBoxes(num) == 0) vacantBoxes.remove(num) else vacantBoxes.sortWith(_ < _)
          }
        }
        case _ =>
      }
    }
    boxSeq
  }
}