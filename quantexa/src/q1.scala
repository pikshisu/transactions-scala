

import TransactionReader._
import scala.collection.mutable.ListBuffer
object q1 extends App {
  val transactionData:List[Transaction] = getTransactions()
  
  // Get days
  val days: List[Int] = transactionData.groupBy(_.transactionDay).keys.toList.sorted
  
  // Sum transactionAmount without losing precision 
  def sumTransactions(ts: List[Transaction]): BigDecimal = 
    (BigDecimal(0) /: ts.map(_.transactionAmount)) (_+_)
  
  // Group by transaction day, map values to sumTransactions
  val records: Map[Int, BigDecimal] = transactionData.groupBy(_.transactionDay).mapValues(sumTransactions(_))
  
  println("Day\tTotal")
  for(td <- days) {
    println(td + "\t" + records(td))
  }
  
}