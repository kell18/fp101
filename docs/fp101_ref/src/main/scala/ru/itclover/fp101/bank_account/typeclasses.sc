

case class BankAccount(balance: Long, spent: Long)

val account1 = BankAccount(1000L, 300L)
val account2 = BankAccount(2000L, 3000L)

// Eq
trait Eqv[T] {
  def compare(a: T, b: T): Int = if (less(a, b)) 1 else
    if (less(b, a)) -1 else 0

  def less(a: T, b: T): Boolean
}

{
  implicit val balanceEqv = new Eqv[BankAccount] {
    override def less(a: BankAccount, b: BankAccount) = a.balance < b.balance
  }

  balanceEqv.less(account1, account2)
}

{
  implicit val relativeEqv = new Eqv[BankAccount] {
    override def less(a: BankAccount, b: BankAccount) = (a.balance - a.spent) <
      (b.balance - b.spent)
  }

  relativeEqv.less(account1, account2)
}


// ToJson



// Semigroup


// Monoid


// Forms


// Top tracks
