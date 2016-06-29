package example

import fpinscala.errorhandling._

/**
  * Created by fredymar on 28/06/16.
  */
object ExercisesOption {

  def main(args: Array[String]) {

    val sep = Seq(1.1,1.2,1.3,1.4,1.5)
    val optionSome = Some(6)
    val optionNone = None

    println("MEAN " + Option.mean(sep))
    println("MAP " + optionSome.map(x => x * 2))
    println("GETORELSE " + optionNone.getOrElse(45))
    println("FLATMAP " + optionSome.flatMap(x  => Some(x * 2)))
    println("ORELSE " + optionNone.orElse(Some(10)))
    println("FILTER " + optionSome.filter(x => x % 5 == 0))



  }

}
