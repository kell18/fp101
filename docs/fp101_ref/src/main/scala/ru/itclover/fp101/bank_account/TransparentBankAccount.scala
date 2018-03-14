package ru.itclover.fp101

object TransparentModel {

  case class BankAccount(balance: Long, overdraft: Long)


  sealed trait Transaction extends (BankAccount => BankAccount)

  case class Withdraw(amount: Long) extends Transaction {
    override def apply(account: BankAccount) = if (amount > account.balance) {
      BankAccount(0, account.overdraft - (amount - account.balance))
    } else {
      BankAccount(account.balance - amount, account.overdraft)
    }
  }

  case class Deposit(amount: Long) extends Transaction {
    override def apply(account: BankAccount) = {
      BankAccount(account.balance + amount, account.overdraft)
    }
  }
}


object TransparentBankAccount extends App {
  import TransparentModel._
  import EqSyntax._
  import EqInstances._

  val account1 = BankAccount(0, 100)
  val account2 = BankAccount(0, 100)

  val transactions = Withdraw(30) :: Withdraw(20) :: Deposit(100) :: Nil
  // a2 = Withdraw(30)(account1) ->
  // a3 = Withdraw(25)(a2) ->
  // a4 = Deposit(100)(a3)
  // FoldLeft!
  val updatedAccount1 = transactions.foldLeft(account1) { case (acc, op) => op(acc) }

  println(updatedAccount1)

  println(balanceAccountEqv.eqv(account1, updatedAccount1)) // false
  println(account1 === updatedAccount1) // false
}
