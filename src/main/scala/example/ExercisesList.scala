package example

import fpinscala.datastructures._

/**
  * Created by fredymar on 28/06/16.
  */
object ExercisesList {

  def main(args: Array[String]) {

    val list = List(1,2,3,4,5,6,7,8,9,10)
    val list2 = List(11,12,13,14,15,16,17,18,19,20)
    val list3 = List(11.1,12.1,13.1,14.1,15.1,16.1,17.1,18.1,19.1,20.1)

    println("DROP: " + List.drop(list,5))
    println("DROPWHILE: " + List.dropWhile(list, (x : Int) => x <= 5))
    println("DROPWHILE2: " + List.dropWhile2(list2)(x => x > 12))
    println("APPEND: " + List.append(list,list2))
    println("INIT: " + List.init(list))
    println("FOLDRIGHT SUM: " + List.foldRight(list,0)((x,y) => x + y))
    println("FOLDRIGHT PRODUCT: " + List.foldRight(list,1)((x,y) => x + y))
    println("FOLDRIGHT LENGHT: " + List.foldRight(list,0)((x,y) => 1 + y))
    println("FOLDLEFT SUM: " + List.foldLeft(list,0)((x,y) => x + y))
    println("FOLDLEFT PRODUCT: " + List.foldLeft(list,1)((x,y) => x + y))
    println("FOLDLEFT LENGHT: " + List.foldLeft(list,0)((x,y) => 1 + y))
    println("REVERTSE: " + List.reverse(list))
    println("APPENDFOLD: " + List.appendFold(list,list2))
    println("INCREMENT: " + List.increment(list))
    println("DOUBLETOSTRING " + List.doubleToString(list3))
    println("MAP " + List.map(list)((x : Int) => x * x))
    println("FILTER " + List.filter(list)((x : Int) => x < 5))
    println("FLATMAP " + List.flatMap(list)(i => List(i,i)))
    println("FILTERFLATMAP " + List.filterViaFlatMap(list)((x : Int) => x > 6))
    println("ADDLIST " + List.addList(list,list2))
    println("ZIPWITH " + List.zipWith(list,list2)((x : Int, y : Int) => x + (y + 2)))

  }

}
