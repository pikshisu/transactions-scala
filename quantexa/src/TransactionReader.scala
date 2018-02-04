

import scala.io.Source
object TransactionReader extends App {
  //Define a case class Transaction which represents a transaction
  case class Transaction(
      transactionId: String,
      accountId: String,
      transactionDay: Int,
      category: String,
      transactionAmount: Double
  )
  
  def getTransactions():List[Transaction] = {
    //The full path to the file to import
    val fileName = "src/transactions.txt"
    
    //The lines of the CSV file (dropping the first to remove the header)
    val transactionslines = Source.fromFile(fileName).getLines().drop(1)
    
    //Here we split each line up by commas and construct Transactions
    val transactions: List[Transaction] = transactionslines.map { line =>
      val split = line.split(',')
      Transaction(split(0), split(1), split(2).toInt, split(3), split(4).toDouble)
    }.toList
    
    transactions
  }
}