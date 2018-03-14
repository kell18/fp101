package ru.itclover.fp101

object OpaqueModel {

  class BankAccount(initBalance: Long, maxOverdraft: Long) {
    var balance: Long = initBalance
    var overdraft: Long = maxOverdraft

    def withdraw(amount: Long): Unit = if (amount > balance) {
      balance = 0
      overdraft -= amount - balance
    } else {
      balance -= amount
    }

    def deposit(amount: Long): Unit = balance += amount

    override def equals(obj: Any) = obj match {
      case other: BankAccount => balance == other.balance && overdraft == other.overdraft
      case _ => false
    }
  }

}


object OpaqueBankAccount extends App {
  import OpaqueModel._

  val account1 = new BankAccount(0, 100)
  val account2 = new BankAccount(0, 100)

  account1.withdraw(30)
  account1.withdraw(20)
  account1.deposit(100)

  println(account1)
  println(account1 == account2) // false
}
