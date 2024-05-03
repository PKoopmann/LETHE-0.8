package uk.ac.man.cs.lethe.internal.application.benchmarks

import scala.concurrent._


/**
 * Special execution context to allow for killing a thread after a timeout
 *
 * taken from
 * http://stackoverflow.com/questions/14449862/kill-or-timeout-a-future-in-scala-2-10
 */

class CustomExecutionContext extends AnyRef with ExecutionContext {
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
