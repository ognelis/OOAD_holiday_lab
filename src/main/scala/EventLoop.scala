/**
 * Created by Ognelis on 9.02.15.
 */

object EventLoop extends App{
  val atm = ATM()
  atm.addCashTape(500)
  atm.addCashTape(1000)
  atm.addCashTape(100)
  atm.addCashTape(5000)
  atm.refillCashTape(100,15)
  atm.refillCashTape(1000,26)
  atm.refillCashTape(5000,5)
  atm.windthrawCash(50000)
  val temp = atm.printRatingAndAmountBankNotes

}