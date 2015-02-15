/**
 * Created by Ognelis on 9.02.15.
 */


/**
 * класс CashTape - денежная кассета, оперирует преимущественно колличеством банкнот;
 * банкомат может иметь несколько денежных кассет.
 * @param bankNoteRating Номинал валюты, по нему как раз и различаются денежные кассеты
 */
class CashTape(val bankNoteRating: Int) {

  //колличество банкнот
  private var bankNoteAmount     : Int = 0

  //узнать колличество банкнот
  def getAmount                  : Int = bankNoteAmount

  //есть ли банкноты вообще в наличии
  def amountBankNotes            : Int = if (bankNoteAmount > 0) bankNoteAmount else 0

  //пополнить денежную кассету
  def refill(amount : Int)       : Unit= bankNoteAmount += amount

  //узнать сумму банкнот денежной кассеты
  def getTotal                   : Int = bankNoteAmount * bankNoteRating

  //получить банкноты из денежной кассеты
  def withdrawBankNotes(amount:Int):Unit= bankNoteAmount -= amount

  //узнать, сколько мы можем отдать банкнот, при их запросе
  def howMuchBankNotesICanGive(cashAmount: Int) : Int = {
    //колличество банкнот которое мы хотим получить
    val amount : Int = cashAmount / bankNoteRating
    //разница между желаемым и действительным коллчиством банкнот
    val difference: Int = amountBankNotes - amount
    //сколько мы отдадим, ориентируясь на реальное колличество банкнот
    val result    : Int = if (difference > 0) amount else amountBankNotes
    //возращем колличесвто банкнот, которое можем отдать
    result
  }

  /*
  def getBankNotes(amount : Int) : Int = {
    val notesAmount = amountBankNotes
    val result      = if (amount > notesAmount) {
                        bankNoteAmount -= notesAmount
                        notesAmount
                      } else {
                        bankNoteAmount -= amount
                        amount
                      }
    result
  }
  */
}

//Объект-компаньон денежная кассета - это синглтон класса CashTape
object CashTape {
  // метод apply - для удобсвта создания экземпляров класса CashTape
  def apply(bankNoteRating : Int) = new CashTape(bankNoteRating)
}
