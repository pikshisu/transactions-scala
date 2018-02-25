

import TransactionReader._
import scala.collection.mutable.HashMap
object q3 extends App {
  val transactionData:List[Transaction] = getTransactions()
  
  // Get days
  val days: List[Int] = transactionData.groupBy(_.transactionDay).keys.toList.sorted
  
  // The data we want for each rolling time window
  case class Record(
      day: Int,
      accountId: String,
      var maximum: Double,
      var sumOfTransactions: Double,
      var numOfTransactions: Int,
      var aaTotal: Double,
      var ccTotal: Double,
      var ffTotal: Double
  )
  
  def statistics(data: List[Transaction], day: Int):List[Record] = {
    // Get the accounts present within the window
    val accounts: List[String] = data.groupBy(_.accountId).keys.toList
    var records: HashMap[String, Record] = HashMap()
    
    // Populate HashMap with an entry for each account
    for(account <- accounts) {
      records += (account -> Record(day, account, 0.0, 0.0, 0, 0.0, 0.0, 0.0))
    }
    
    // Update records
    for(t <- data) {
      var accountInfo = records(t.accountId)
      
      if(t.transactionAmount > accountInfo.maximum) 
        accountInfo.maximum = t.transactionAmount
      
      accountInfo.sumOfTransactions += (t.transactionAmount)
      accountInfo.numOfTransactions += 1
      
      t.category match {
        case "AA" => accountInfo.aaTotal += t.transactionAmount
        case "CC" => accountInfo.ccTotal += t.transactionAmount
        case "FF" => accountInfo.ffTotal += t.transactionAmount
        case _ => 
      }
    }
    
    // Return the records for each account
    return records.values.toList
  }
  
  println("Day\tAccID\tMax\tAvg\tAA.tv\tCC.tv\tFF.tv")
  for(i <- 6 to days.last) {
    val window: List[Transaction] = transactionData.filter(t => i-5 until i contains t.transactionDay)
    val recordsForDay: List[Record] = statistics(window, i)
    
    for(r <- recordsForDay) {
      val average = r.sumOfTransactions / r.numOfTransactions
      printf("%d\t%s\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\n", r.day, r.accountId, r.maximum, average, r.aaTotal, r.ccTotal, r.ffTotal)
    }
  }
  
}