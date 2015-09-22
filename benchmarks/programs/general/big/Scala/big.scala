import java.util.Random

import scala.actors.Actor
import scala.actors.Actor._

abstract class Message
case class Ping(id:Int) extends Message
case class Pong(id:Int) extends Message
case class Stop(id:Int) extends Message

case object Exit extends Message

object big {
  def run(n: Int) {
    val workerAmount = n
    val numMessages = 16000
    val rand = new Random(System.currentTimeMillis())

    val chunkSize = 1000
    var counter = 1
    val chunks = if (workerAmount >= chunkSize) workerAmount / chunkSize else 1
    var arraySize = 0

    val sinkActor = new SinkActor(workerAmount)
    sinkActor.start()

    while (counter < chunks + 1) {
      if (chunkSize * (counter + 1) > workerAmount)
        if (workerAmount > chunkSize)
          arraySize = chunkSize + workerAmount % chunkSize
        else
          arraySize = workerAmount
      else
        arraySize = chunkSize

      val bigActors = Array.tabulate[BigActor](arraySize)(i => {
        val loopActor = new BigActor(i, numMessages, sinkActor, rand)
        loopActor.start()
        loopActor
      })

      sinkActor.addNeighbors(bigActors)
      bigActors.foreach(loopActor => {
        loopActor.setNeighbors(bigActors)
      })

      bigActors.foreach(loopActor => {
        loopActor ! Pong(-1)
      })
      counter = counter + 1
    }
  }

  def main(args: Array[String]) {
    val n = if (args.isEmpty) 256 else Integer.parseInt(args(0))
    run(n)
  }
}

class BigActor(id: Int, numMessages: Int, sinkActor: SinkActor, rand: Random) extends Actor {
  var numPings = 0
  var expPinger = -1
  var neighbors = null : Array[BigActor]

  def sendPing() {
    val target = rand.nextInt(neighbors.size)
    val targetActor = neighbors(target)

    expPinger = target
    targetActor ! Ping(id)
  }

  def setNeighbors(neighbors : Array[BigActor]) {
    this.neighbors = neighbors
  }

  def act() {
    var pingsLeft = numMessages
    loop {
      react {
        case Ping(id) =>
          sender ! Pong(this.id)
        case Pong(id) =>
          if(id != expPinger)
            println("ERROR: Expected: " + expPinger + " but received ping from " + id + "\n")
          if(pingsLeft == 0) {
            sinkActor ! Exit
          } else {
            sendPing()
            pingsLeft -= 1
          }
        case Exit =>
          exit()
      }
    }
  }
}

class SinkActor(numWorkers: Int) extends Actor {
  var numMessages = 0
  var neighbors = Array.empty[Array[BigActor]]

  def addNeighbors(neighbors: Array[BigActor]) {
    this.neighbors = neighbors +: this.neighbors 
  }

  def act() {
    var pongCount = 0
    loop {
      react {
        case Exit =>
          numMessages += 1
          if(numMessages == numWorkers) {
            neighbors.foreach(neighborArray => neighborArray.foreach(loopWorker => loopWorker ! Exit))
            println("Everything should be done now!")
            exit()
          }
      }
    }
  }
}
