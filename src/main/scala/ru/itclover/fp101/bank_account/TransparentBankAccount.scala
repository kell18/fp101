package ru.itclover.fp101.bank_account

import ru.itclover.fp101.bank_account.TransparentModel.BankAccount


object TransparentModel {
  // Make BankAccount ADT
  case class BankAccount(balance: Long, overdraft: Long)
}


object Transactions {
  // Also extract methods to data-level as ADT
  trait Transaction extends (BankAccount => BankAccount)

  case class Withdraw(amount: Long) extends Transaction {
    override def apply(account: BankAccount) = if (amount > account.balance) {
      BankAccount(0, account.overdraft - (amount - account.balance))
    } else {
      BankAccount(account.balance - amount, account.overdraft)
    }
  }

  case class Deposit(amount: Long) extends Transaction {
    override def apply(account: BankAccount) = BankAccount(account.balance + amount, account.overdraft)
  }

  case class IncreaseOverdraft(amount: Long) extends Transaction {
    override def apply(account: BankAccount) = BankAccount(account.balance, account.overdraft + amount)
  }
}


object EqvTypeclass {
  // Extract equivalence logic to typeclass
  trait Eqv[T] {
    def eqv(a: T, b: T): Boolean
  }

  // Extends generic type T with method of implicit class
  // Adds possibility to use `A === B` syntax
  implicit class EqvSyntax[T](val a: T) extends AnyVal {
    def ===(b: T)(implicit equivalence: Eqv[T]) = equivalence.eqv(a, b)
  }

  object EqvInstances {
    implicit val accountEqv = new Eqv[BankAccount] {
      override def eqv(a: BankAccount, b: BankAccount) = a.balance == b.balance
    }
  }
}


object TransparentBankAccount extends App {
  import TransparentModel._
  import Transactions._
  import EqvTypeclass._
  import EqvTypeclass.EqvSyntax
  import EqvTypeclass.EqvInstances
  import EqvTypeclass.EqvInstances._

  val account1 = BankAccount(0, 100)
  val account2 = BankAccount(0, 100)

  val transactions = Withdraw(30) :: Withdraw(20) :: Deposit(100) :: IncreaseOverdraft(500) :: Nil
  // Apply transactions to account1
  // I.e. concise version of: `a1 = transactions(0)(account1) -> a2 = transactions(1)(a1) -> a3 = transactions(2)(a3) -> ...`
  val resultAccount1  = transactions.foldLeft(account1) { case (acc, transaction) => transaction(acc) }

  println(account1)
  println(resultAccount1)

  println(account1.===(account2))
  // Same as:
  println(account1 === resultAccount1)
}
