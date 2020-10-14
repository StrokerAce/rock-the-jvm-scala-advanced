package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {

//  // JVM threads
//
//  val aThread = new Thread(new Runnable {
//    override def run(): Unit = println("running in parallel")
//  })
//
//  aThread.start() // gives the signal to the JVM to start a JVM thread
//  // Create a JVM Thread => OS Thread
//
//  aThread.join() // Blocks until Thread finished running
//
//  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("Hello")))
//  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("Goodbye")))
//
//  threadHello.start
//  threadGoodbye.start
//
//  // executors
//  val pool = Executors.newFixedThreadPool(10)
//
//  pool.execute(() => println("Something in the thread pool"))
//
//  pool.execute(() => {
//    Thread.sleep(1000)
//    println("Done after 1 second")
//  })
//
//  pool.execute(() => {
//    Thread.sleep(1000)
//    println("Almost done")
//    Thread.sleep(1000)
//    println("Done after 2 seconds")
//  })
//
//  pool.shutdown()
//
//  //pool.execute(() => println("Should not appear"))// Throws an exception in calling thread
//
//  //pool.shutdownNow()
//
//  println(pool.isShutdown)
//
//  def runInParallel() = {
//
//    var x = 0
//
//    val thread1 = new Thread(() => x = 1)
//    val thread2 = new Thread(() => x = 1)
//
//    thread1.start
//    thread2.start
//
//    println(x)
//  }
//
//  // Race condition
//  for (_ <- 1 to 10000) runInParallel()

  class BankAccount(var amount: Int) {
    override def toString: String = "" + amount
  }

  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount -= price
    //println(s"I've bought $thing")
    //println(s"My account is now $account")
  }

  for (_ <- 1 to 10000) {
    val account = new BankAccount(50000)
    val thread1 = new Thread(() => buy(account, "shoes", 3000))
    val thread2 = new Thread(() => buy(account, "iphone12", 4000))
    thread1.start
    thread2.start
    Thread.sleep(10)
    if(account.amount != 43000) println(account)
  }

  // Option #1 use synchronized
  def buySafe(account: BankAccount, thing: String, price: Int) = {
   account.synchronized{
     account.amount -= price
     println(s"I've bought $thing")
     println(s"My account is now $account")
   }
  }

  // option #2 @volatile


}
