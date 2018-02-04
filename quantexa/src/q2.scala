

import TransactionReader._
import scala.collection.mutable.HashMap
object q2 extends App {
  val transactionData:List[Transaction] = getTransactions()
  
  // HashMap -> effectively constant lookup + add
  // Maps from accountId -> tuple | tuple(sumOfTransactionType, occurrenceOfTransactionType ...)
  var records:HashMap[String, (BigDecimal, Int, 
      BigDecimal, Int, 
      BigDecimal, Int, 
      BigDecimal, Int, 
      BigDecimal, Int, 
      BigDecimal, Int, 
      BigDecimal, Int)] = HashMap()
  
  for(t <- transactionData) {
    // Add unvisited accounts to HashMap
    if(!records.contains(t.accountId)) {
      val zero:BigDecimal = 0.0
      var empty = (zero, 0, 
          zero, 0, 
          zero, 0, 
          zero, 0, 
          zero, 0, 
          zero, 0, 
          zero, 0)
      records += (t.accountId -> empty)
    }
    
    var accountInfo = records(t.accountId)
    
    // Create new tuple by adding transactionAmount (and occurrenceOfTransactionType + 1)
    t.category match {
      case "AA" => {
        accountInfo = accountInfo.copy(_1 = accountInfo._1 
            + t.transactionAmount, _2 = accountInfo._2 + 1)
      }
      case "BB" => {
        accountInfo = accountInfo.copy(_3 = accountInfo._3 
            + t.transactionAmount, _4 = accountInfo._4 + 1)
      }
      case "CC" => {
        accountInfo = accountInfo.copy(_5 = accountInfo._5 
            + t.transactionAmount, _6 = accountInfo._6 + 1)
      }
      case "DD" => {
        accountInfo = accountInfo.copy(_7 = accountInfo._7 
            + t.transactionAmount, _8 = accountInfo._8 + 1)
      }
      case "EE" => {
        accountInfo = accountInfo.copy(_9 = accountInfo._9 
            + t.transactionAmount, _10 = accountInfo._10 + 1)
      }
      case "FF" => {
        accountInfo = accountInfo.copy(_11 = accountInfo._11 
            + t.transactionAmount, _12 = accountInfo._12 + 1)
      }
      case "GG" => {
        accountInfo = accountInfo.copy(_13 = accountInfo._13 
            + t.transactionAmount, _14 = accountInfo._14 + 1)
      }
    }
    
    // Update HashMap
    records.update(t.accountId, accountInfo)
  }
  
  println("Account\tAA Avg\tBB Avg\tCC Avg\tDD Avg\tEE Avg\tFF Avg\tGG Avg")
  for((account, info) <- records) {
    print(account + "\t")
    var avg:BigDecimal = if(info._1.compareTo(0.0) > 0) info._1/info._2 else 0.0
    print(f"$avg%.2f" + "\t")
    
    avg = if(info._3.compareTo(0.0) > 0) info._3/info._4 else 0.0
    print(f"$avg%.2f" + "\t")
    
    avg = if(info._5.compareTo(0.0) > 0) info._5/info._6 else 0.0
    print(f"$avg%.2f" + "\t")
    
    avg = if(info._7.compareTo(0.0) > 0) info._7/info._8 else 0.0
    print(f"$avg%.2f" + "\t")
    
    avg = if(info._9.compareTo(0.0) > 0) info._9/info._10 else 0.0
    print(f"$avg%.2f" + "\t")
    
    avg = if(info._11.compareTo(0.0) > 0) info._11/info._12 else 0.0
    print(f"$avg%.2f" + "\t")
    
    avg = if(info._13.compareTo(0.0) > 0) info._13/info._14 else 0.0
    println(f"$avg%.2f" + "\t")
  }
  
}