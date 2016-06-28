package fpinscala.datastructures

/**
  * Created by fredymar on 27/06/16.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons [+A](head: A, tail : List[A]) extends List[A]

object List{

  def sum(list : List[Int]) : Int = list match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(list : List[Int]) : Int = list match {
    case Nil => 0
    case Cons(0.0,_) => 0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A] (as: A*) : List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head,apply(as.tail:_*))
  }

  def tail[A](list : List[A]) : List[A] = list match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def setHead[A](list : List[A],a : A) : List[A] = list match {
    case Nil => Nil
    case Cons(x,xs) => Cons(a,xs)
  }

  def drop[A](list : List[A], n : Int) : List[A] = list match {
    case Nil => list
    case Cons(x,xs) if(n > 0) => drop(xs,n-1)
    case _ => list
  }

  def dropWhile[A](list : List[A], f : A => Boolean) : List[A] = list match {
    case Nil => list
    case Cons(x,xs) if f(x) => dropWhile(xs,f)
    case Cons(x,xs) => Cons(x,dropWhile(xs,f))
  }

  def dropWhile2[A](list : List[A]) (f : A => Boolean) : List[A] = list match {
    case Nil => list
    case Cons(x,xs) if f(x) => dropWhile(xs,f)
    case Cons(x,xs) => Cons(x,dropWhile(xs,f))
  }

  def append[A] (listA : List[A], listB : List[A]) : List[A] = listA match{
    case Nil => listB
    case Cons(x,xs) => Cons(x,append(xs,listB))
  }

  def init[A](list : List[A]) : List[A] = list match {
    case Nil => list
    case Cons(x,xs) if(xs.equals(Nil)) => init(xs)
    case Cons(x,xs) => Cons(x,init(xs))
  }

  def foldRight[A,B](list : List[A], z : B)(f : (A,B) => B) : B = list match {
    case Nil => z
    case Cons(x,xs) => f(x,foldRight(xs,z)(f))
  }

  def foldLeft[A,B](list : List[A],z : B) (f: (B,A) => B) : B = list match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, List[A]())((x, y) => Cons(y, x))
  }

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B ={
    foldRight(as,z)((a : A , b : B) => f(b,a))
  }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B ={
    foldLeft(as,z)((b : B , a : A ) => f(a,b))
  }

  def appendFold[A](a1: List[A], a2: List[A]): List[A] = {
    //foldRight(a1 , a2)((x: A, y: List[A]) => Cons(x, y))
    foldLeft(reverse(a1), a2)((b: List[A], a: A) => Cons(a, b))
  }

  def concat[A](list: List[List[A]]): List[A] = {
    //foldRight(list, Nil: List[A])(append)
    foldLeft(reverse(list),Nil : List[A]) (append)
  }

  def increment(list: List[Int]): List[Int] = {
    //foldRight(list, Nil: List[Int]) ((x : Int, acc : List[Int]) => Cons( x+1 , acc))
    foldLeft(reverse(list), Nil : List[Int]) ((acc: List[Int], x : Int) => Cons(x+1,acc))
  }

  def doubleToString(list: List[Double]): List[String] = {
    //foldRight(list, Nil : List[String]) ((x : Double, acc : List[String]) => Cons(x.toString,acc))
    foldLeft(reverse(list), Nil : List[String]) ((acc : List[String], x : Double) => Cons(x.toString,acc))
  }

  def map[A,B](list : List[A])(f: A => B): List[B] = {
    //foldRight(list, Nil : List[B])((x : A, y : List[B])=> Cons(f(x),y))
    foldLeft(reverse(list), Nil : List[B])((y : List[B], x : A) => Cons(f(x),y))
  }

  def filter[A](list: List[A])(f: A => Boolean): List[A] = {
    //foldRight(list, Nil : List[A])((x : A, acc : List[A]) => if(f(x)) Cons(x,acc) else acc)
    foldLeft(reverse(list), Nil : List[A])((acc: List[A], x: A) => if (f(x)) Cons(x,acc) else acc)
  }

  def flatMap[A,B] (list : List[A])(f: A => List[B]): List[B] ={
    //foldRight(list, Nil : List[B])((x : A, acc: List[B]) => append(f(x),acc))
    //foldLeft(reverse(list), Nil : List[B])((acc : List[B], x : A) => append(f(x),acc))
    concat(map(reverse(list))(f))
  }

  def filterViaFlatMap[A](list : List[A])(f: A => Boolean): List[A] = {
    flatMap(list)((x : A) => if (f(x)) List(x) else Nil)
  }

  def addList(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
    case (_,Nil) => list1
    case (Nil,_) => list2
    case (Cons(x,y), Cons(z,a)) => Cons(x + z, addList(y,a))
  }

  def zipWith[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] = (list1, list2) match {
    case (_,Nil) => Nil
    case (Nil,_) => Nil
    case (Cons(x,y),Cons(z,a)) => Cons(f(x,z),zipWith(y,a)(f))
  }


}