

import TransactionReader._
import scala.collection.mutable.ListBuffer
object q1 extends App {
  val transactionData:List[Transaction] = getTransactions()
  
  // ListBuffer -> Append in constant time
  var records:ListBuffer[(Int, BigDecimal)] = ListBuffer()
  var day:Int = transactionData(1).transactionDay
  var total:BigDecimal = transactionData(1).transactionAmount
  
  for(i <- 1 until transactionData.length) {
    // Append to ListBuffer if day changes
    if(day != transactionData(i).transactionDay) {
      records += ((day, total))
      day = transactionData(i).transactionDay
      total = 0.0
    }
    total += transactionData(i).transactionAmount
  }
  
  // Append final day
  records += ((day, total))
  
  println("Day\tTotal")
  for(t <- records) {
    println(t._1 + "\t" + t._2)
  }
  
}