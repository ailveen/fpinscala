package fpinscala.iomonad.io1

case class Player(name: String, score: Int)

object IOPlayground extends App {

  def contest_impure(p1: Player, p2: Player) : Unit =
    if (p1.score > p2.score)
      Console println s"""${p1.name} is the winner with score ${p1.score}"""
    else if (p2.score > p1.score)
      Console println s"""${p2.name} is the winner with score ${p2.score}"""
    else
      Console println s"""It is a draw between p1 and p2"""

  def winnerMessage(p1: Player, p2: Player) : String =
    winnerMessage(winner(p1, p2))

  def winnerMessage(p: Option[Player]) : String =
    p match {
      case Some(p) => s"""${p.name} is the winner with score ${p.score}"""
      case None => s"""It is a draw"""
    }

  def consolePrint(msg: String): Unit = Console println msg

  def winner(p1: Player, p2: Player) : Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  consolePrint(winnerMessage(Player("A", 15), Player("B", 7)))

  // via IO Monad
  def consolePrintMonad(msg: String) : IO[Unit] = IO {
    Console println msg
  }

  consolePrintMonad(winnerMessage(Player("A", 15), Player("B", 7))) run
}

object TempConverter extends App {
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def prompt: IO[Unit] = IO ( Console print s"""Enter temp in Fahrenheit: """ )

  def readPrompt: IO[String] = IO ( io.StdIn.readLine )

  def consolePrintMonad(msg: Any) : IO[Unit] = IO {
    Console println msg
  }

  val proc = for {
    _ <- prompt
    fs <- readPrompt
    _ <- consolePrintMonad(s"""Equiv Temp in Celsius: ${fahrenheitToCelsius(fs.toDouble)}""")
  } yield {}

  proc run
}




