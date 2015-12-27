package fpinscala.iomonad.io2

object IOPlayground extends App {

  def printLine(s: String): TailRec[Unit] =
    Suspend(() => println(s))

  val p = TailRec.forever(printLine("Still going..."))

  TailRecInterpreter run p
}
