package fpinscala.errorhandling

/**
  * Created by fredymar on 28/06/16.
  */
sealed trait Option[+A]{

  def map[B](f : A => B) : Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B] (f : A => Option[B]) : Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B >: A](ob: => Option[B]) : Option[B] = this match {
    case None => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean) : Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }

}

case class Some[+A] (get : A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs : Seq[Double]) : Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
}
