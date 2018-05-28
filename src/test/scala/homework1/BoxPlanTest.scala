package homework1


import org.scalatest.{FlatSpecLike, Matchers}


class BoxPlanTest extends FlatSpecLike with Matchers {
  "BoxPlan.plan" should "return seq of boxes" in {
    BoxPlan.plan(Seq(
      Guitar(),
      TV(5),
      Guitar(),
      Shoes(),
      PlayStation(),
      Shoes(),
      Cat(),
      Shoes(),
      Shoes(),
      Dish(),
      Shoes(),
      Easel(),
      Uculele(),
      Dish()
    )) should contain theSameElementsAs Seq(GuitarBox(), BigBox(), GuitarBox(),PlayStationBox(), BasicBox(), EaselBox())
  }

  it should "throw exception" in {
    assertThrows[Exception](
      BoxPlan.plan(Seq(TV(11)))
    )
  }

  it should "return empty Seq" in {
    BoxPlan.plan(Seq()) shouldBe empty
  }
}