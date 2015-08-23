import java.util.concurrent.Executors

import fpinscala.parallelism.Actor

val S = Executors.newFixedThreadPool(5)

val echoer = Actor[String](S) {
  msg => Console println s"Got message: '$msg'"
}

echoer ! "hello"

Console println "x"