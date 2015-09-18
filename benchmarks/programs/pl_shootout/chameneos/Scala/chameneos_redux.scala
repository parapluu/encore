/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   Contributed by Eric Willigers
   Port of Java implementation by Michael Barker and Luzius Meisser
   Slightly modified
*/

object Colours extends Enumeration {
   val Blue = Value("blue")
   val Red = Value("red")
   val Yellow = Value("yellow")
}

import Colours.{Blue, Red, Yellow, Value => Colour}

final class Creature(place: MeetingPlace, var colour: Colour) extends Runnable {
   val id = System.identityHashCode(this)
   var sameCount = 0
   var count = 0

   def run() = try {
      while (true) {
         val p = place.meet(id, colour)
         colour = p.colour
         if (p.sameId)
            sameCount += 1
         count +=1
      }
   } catch {
      case _: Exception => ()
   }

   override def toString = String.valueOf(count)+" "+chameneosredux.getNumber(sameCount)
}

final class MeetingPlace(var meetingsLeft: Int) {
   var firstColour: Option[Colour] = None
   var firstId = 0
   var current: Future = _

   def meet(id: Int, c: Colour) = synchronized {
      if (meetingsLeft == 0) {
         throw new Exception("Finished")
      } else {
         if (firstColour.isEmpty) {
            firstColour = Some(c)
            firstId = id
            current = new Future()
         } else {
            current.setItem(new Pair(id == firstId, chameneosredux.doCompliment(c, firstColour.get)))
            firstColour = None
            meetingsLeft -= 1
         }

         current
      }
   }.getItem()
}

final class Future {
   @volatile var p: Pair = _

   def getItem() = {
      while (p == null)
         Thread.`yield`()   
      p
   }

   def setItem(_p: Pair) {
      this.p = _p
   }
}

final case class Pair(sameId: Boolean, colour: Colour)

object chameneosredux {
   def doCompliment(c1: Colour, c2: Colour) = (c1, c2) match {
      case (Blue, Blue) => Blue   
      case (Blue, Red) => Yellow   
      case (Blue, Yellow) => Red   
      case (Red, Blue) => Yellow
      case (Red, Red) => Red
      case (Red, Yellow) => Blue   
      case (Yellow, Blue) => Red   
      case (Yellow, Red) => Blue
      case (Yellow, Yellow) => Yellow
   }

   def run(n: Int, colours: Colour*) {
      val place = new MeetingPlace(n)
      colours.foreach { c => print(" "+c) }
      val creatures = colours.map { new Creature(place, _) }.toArray
      println()
      val ts = creatures.map { new Thread(_) }
      ts.foreach { _.start() }
      ts.foreach { _.join() }
      creatures.foreach { println(_) }
      println(getNumber(creatures.foldLeft(0){_ + _.count}))
      println()
   }

   def intToColour(num : Int) = num match {
     case 0 => Blue
     case 1 => Yellow
     case 2 => Red
   }

   def runCreaturePar(n: Int, creatureAmount : Int) {
      val place = new MeetingPlace(n)
      val creatures = new Array[Creature](creatureAmount)
      for(i <- 0 to creatures.length-1) {
        creatures(i) = new Creature(place, intToColour(i % 3))
      }
      println()
      val ts = creatures.map { new Thread(_) }
      ts.foreach { _.start() }
      ts.foreach { _.join() }
      println(getNumber(creatures.foldLeft(0){_ + _.count}))
      println()
   }

   def main(args: Array[String]) {
      val n = if (args.isEmpty) 600 else Integer.parseInt(args(0))
      var numChameneos = 4
      if(args.length == 2)
        numChameneos = Integer.parseInt(args(1))
      printColours()
      println()
      if(args.length == 2) {
        runCreaturePar(n, numChameneos)
      } else {
        run(n, Blue, Red, Yellow)
        run(n, Blue, Red, Yellow, Red, Yellow,
          Blue, Red, Yellow, Red, Blue)
      }
   }

   val Numbers = Array[String]("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

   def getNumber(n: Int) = String.valueOf(n).toList.map { ch => Numbers(Character.getNumericValue(ch)) } .mkString(" ")

   def printColours() {
      printColours(Blue, Blue)
      printColours(Blue, Red)
      printColours(Blue, Yellow)
      printColours(Red, Blue)   
      printColours(Red, Red)
      printColours(Red, Yellow)
      printColours(Yellow, Blue)
      printColours(Yellow, Red)
      printColours(Yellow, Yellow)
   }

   def printColours(c1: Colour, c2: Colour) {
      println(c1+" + "+c2+" -> "+doCompliment(c1, c2))   
   }
}
