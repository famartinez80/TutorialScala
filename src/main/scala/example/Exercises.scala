package example

import fpinscala.datastructures._

/**
  * Created by fredymar on 24/06/16.
  */
object Exercises {

  def abs(n :Int) : Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n : Int) : Int ={
    @annotation.tailrec
    def go(n : Int, acc : Int) : Int = {
      if(n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n,1)
  }


  def fibonacci(n : Int) : Int ={
    @annotation.tailrec
    def loop(n : Int, bef : Int, aft : Int) : Int = {
      if (n <= 0) aft
      else loop(n - 1, aft, bef + aft)
    }
    loop(n,0,1)
  }

  private def formAbs(x : Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x,abs(x))
  }

  private def formFatorial(n : Int) = {
    val msg = "The factorial value of %d is %d"
    msg.format(n,factorial(n))
  }

  def formatResult(name : String, n : Int, f: Int => Int) = {
    val msg = "The %s value of %d is %d"
    msg.format(name,n,f(n))
  }

  def findFirst[A](ss : Array[A], p : A => Boolean): Int = {
    @annotation.tailrec
    def loop (n : Int): Int ={
      if(n >= ss.length) -1
      else if (p(ss(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def isSorted[A] (as : Array[A], ordered: (A,A) => Boolean) : Boolean = {
    @annotation.tailrec
    def loop (n : Int): Boolean ={
      if(n + 2 > as.length) true
      else if (ordered(as(n),as(n+1))) loop(n+1)
      else false
    }
    loop(0)
  }

  def curry [A,B,C] (f: (A,B) => C) : A => (B => C) = {
    (a : A) => (b : B) => f(a,b)
  }

  def uncurry[A,B,C] (f: A => B => C): (A,B) => C = {
    (a : A, b : B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B ): A => C ={
    (a : A) => f(g(a))
  }


  def main(args: Array[String]) {
    val list = List(1,2,3,4,5,6,7,8,9,10)
    val list2 = List(11,12,13,14,15,16,17,18,19,20)
    val list3 = List(11.1,12.1,13.1,14.1,15.1,16.1,17.1,18.1,19.1,20.1)
    println(formAbs(-42))
    println(formFatorial(7))
    println(formatResult("Factorial",7,factorial))
    println(findFirst(Array("Carlos","Fredy","Andres","Cesar"), (x : String) => x == "Carlos"))
    println(findFirst(Array(1,2,3,4), (x : Int) => x == 9))
    println(isSorted(Array(1,2), (x : Int,y: Int) => x >= y))

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


