package uk.ac.man.cs.lethe.internal.tools.threads

import scala.concurrent._
import scala.concurrent.duration._


class CustomExecutionContext extends AnyRef with ExecutionContext {
  import ExecutionContext.Implicits.global
  @volatile var lastThread: Option[Thread] = None
  override def execute(runnable: Runnable): Unit = {
    ExecutionContext.Implicits.global.execute(new Runnable() {
      override def run() {
        lastThread = Some(Thread.currentThread)
        runnable.run()
      }
    })
  }
  override def reportFailure(t: Throwable): Unit = ???
}    
