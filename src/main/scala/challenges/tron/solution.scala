

object Solution {
  def main(args: Array[String]): Unit = { 
    val player = scala.io.StdIn.readLine
    val pos = scala.io.StdIn.readLine
    val board = new Array[String](15)
    for (i <- 0 until 15) {
      board.update(i, scala.io.StdIn.readLine)
    }
    val out: Direction = Functions.nextMove(State(player, pos, board))
    println(out.toString)
  }

  object Functions extends Util {
    def nextMove(state: State): Direction = {
      if (targetValid(RIGHT, state))
        RIGHT
      else if (targetValid(LEFT, state))
        LEFT
      else if (targetValid(UP, state))
        UP
      else
        DOWN
    }

    def positionEmpty(position: Position, state: State): Boolean = position match {
      case InvalidPosition => false
      case ValidPosition(xx, yy) =>
        debug(s"XX is $xx")
        debug(s"YY is $yy")
        debug(s"board is ${state.board(yy)(xx)}")
        state.board(yy)(xx) match {
          case '-' => true
          case _ => false
        }
    }

    def targetValid(direction: Direction, state: State): Boolean = {
      val positions: Array[Int] = state.position.split(" +").map(_.toInt)
      val myPosition: Position = state.player match {
        case "r" => ValidPosition(positions(1), positions(0))
        case "g" => ValidPosition(positions(3), positions(2))
      }
      val res = direction match {
        case RIGHT => positionEmpty(myPosition.right, state)
        case LEFT => positionEmpty(myPosition.left, state)
        case UP => positionEmpty(myPosition.up, state)
        case DOWN => positionEmpty(myPosition.down, state)
      }
      res
    }
  }
}

case class State(player: String, position: String, board: Array[String])

trait Position {
  def right: Position
  def left: Position
  def up: Position
  def down: Position
}
case class ValidPosition(xx: Int, yy: Int) extends Position {
  val min = 1
  val max = 13

  def right: Position = {
    if (xx + 1 > max)
      InvalidPosition
    else 
      ValidPosition(xx + 1, yy)
  }
  def left: Position = {
    if (xx - 1 < min)
      InvalidPosition
    else
      ValidPosition(xx - 1, yy)
  }
  def up: Position = {
    if (yy - 1 < min)
      ValidPosition(xx, yy - 1)
    else
      InvalidPosition
  }
  def down: Position = {
    if (yy + 1 > max)
      ValidPosition(xx, yy + 1)
    else
      InvalidPosition
  }
}
case object InvalidPosition extends Position { self =>
  def right: Position = self
  def left: Position = self
  def up: Position = self
  def down: Position = self
}

trait Direction
case object RIGHT extends Direction
case object LEFT extends Direction
case object UP extends Direction
case object DOWN extends Direction

/**
trait GridMark
case object Dash extends GridMark {
  override def toString = "-"
}
case object Pound extends GridMark {
  override def toString = "#"
}
case object PlayerR extends GridMark {
  override def toString = "r"
}
case object PlayerG extends GridMark {
  override def toString = "g"
}
*/

trait Util {
  def debug(str: String): Unit = System.err.println(str)
}
