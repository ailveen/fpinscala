package fpinscala.parallelism

import java.util.concurrent.CountDownLatch

object XCountdownLatch extends App {


  val cl = new CountDownLatch(2)

  val worker = new Worker(cl)
  Console println "Running worker"
  worker.run()
  cl.await()

  Console println "Proceeding..."
}

class Worker(latch : CountDownLatch) extends Runnable {
  override def run(): Unit = {
    Console println "worker running..."
    Thread.sleep(3000)
    Console println "Worker done, calling countdown..."
    latch.countDown()
  }
}