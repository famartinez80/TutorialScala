package example

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

    println(formAbs(-42))
    println(formFatorial(7))
    println(formatResult("Factorial",7,factorial))
    println(findFirst(Array("Carlos","Fredy","Andres","Cesar"), (x : String) => x == "Carlos"))
    println(findFirst(Array(1,2,3,4), (x : Int) => x == 9))
    println(isSorted(Array(1,2), (x : Int,y: Int) => x >= y))
  }
}


