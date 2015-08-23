import java.util.concurrent.{ExecutorService, Executors}
import fpinscala.parallelism.Nonblocking.Par

val p = Par.parMap(List.range(1, 100000))(math.sqrt(_))

val fixedThreadPool: ExecutorService = Executors.newFixedThreadPool(10)
val x = Par.run(fixedThreadPool)(p)

def func(x : Int) : Int = {
  throw new RuntimeException
}

// don't enable or thread will hang...
//val te = Par.run(fixedThreadPool)(Par.asyncF(func)(1))