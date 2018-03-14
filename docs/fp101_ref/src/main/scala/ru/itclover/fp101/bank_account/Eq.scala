package ru.itclover.fp101


trait Eq[T] {
  def eqv(a: T, b: T): Boolean
}

object EqInstances {
  import TransparentModel._

  implicit val balanceAccountEqv = new Eq[BankAccount] {
    override def eqv(a: BankAccount, b: BankAccount) = a.balance == b.balance
  }
}

object EqSyntax {
  implicit class EqOps[T](a: T)(implicit eq: Eq[T]) {
    def ===(b: T) = eq.eqv(a, b)
  }
}