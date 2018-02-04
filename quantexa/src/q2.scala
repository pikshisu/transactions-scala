

import TransactionReader._
object q2 extends App {
  val transactionData:List[Transaction] = getTransactions()
  
  // Get accounts and categories
  val accounts: List[String] = transactionData.groupBy(_.accountId).keys.toList
  val categories: List[String] = transactionData.groupBy(_.category).keys.toList.sorted
  
  // Existing (account, category) pairs are mapped to an average
  val result = transactionData.groupBy(t => (t.accountId, t.category)).map {
    case (key, ts) =>
      val values = ts.map(_.transactionAmount)
      key -> values.sum / values.size
  }
  
  var headers = "AccID"
  for(category <- categories) {
    headers += "\t" + category + ".av"
  }
  println(headers)
  
  for(account <- accounts) {
    print(account)
    
    for(category <- categories) {
      if(result.contains(account, category)) {
        val average = result(account, category)
        print("\t" + f"$average%.2f")
      } else {
        print("\t" + 0.0)
      }
    }
    println()
  }
  
}