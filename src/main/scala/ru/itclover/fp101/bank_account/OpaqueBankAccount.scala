package ru.itclover.fp101.bank_account

/**
  * Does this program outline have any conceptual problems? How would you refactor it?
  */
object OpaqueModel {

  class BankAccount(initBalance: Long, maxOverdraft: Long) {
    private var balance: Long = initBalance
    private var overdraft: Long = maxOverdraft

    def withdraw(amount: Long): Unit = if (amount > balance) {
      balance = 0
      overdraft -= amount - balance
    } else {
      balance -= amount
    }

    def deposit(amount: Long): Unit =
      balance += amount


    def eqv(other: BankAccount): Boolean =
      balance == other.getBalance && overdraft == other.getOverdraft


    def getBalance: Long = balance

    def getOverdraft: Long = overdraft
  }
}


object OpaqueBankAccount extends App {
  import OpaqueModel._

  val account1 = new BankAccount(0, 100)
  val account2 = new BankAccount(0, 100)

  account1.withdraw(30)
  account1.withdraw(20)
  account1.deposit(100)

  println(account1.getBalance)
  println(account1 eqv account2)
}
