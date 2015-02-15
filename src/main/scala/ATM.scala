import scala.collection.mutable.ArrayBuffer
/**
 * Created by Ognelis on 9.02.15.
 */


/**
 * Банкомат
 */
class ATM {

  // одному банкомату, может принадлежать 1 и более денежных кассет
  val cashTapes = ArrayBuffer[CashTape]()

  /*
    нижестоящие переменные используются для создания ЦЕПОЧКИ ОТВЕСТВЕННОСТИ.
    Главная идея сосоит в том, что используется частичные функции (partial functions).
    Частичная функция — это функция определенная на подмножестве возможных значений своих аргументов.
   */

  //Сумму которую мы хотим получить с банкомата
  var cash : Int = 0

  //Частичная функция
  type EventHandler  = PartialFunction[Event, Unit]

  //Частиная функиця по умолчанию, ничего не делает при ее вызове
  val defaultHandler : EventHandler = PartialFunction(_ => ())

  //Цепочка для выдачи наличных
  val cashEvent      = cashRubHandler(cash).orElse(defaultHandler)

  //Цепочка для выдачи наличных мелкими купюрами
  val pettyCashEvent = pettyCashRubHandler(cash).orElse(defaultHandler)

  //Присоедниям к банкомату денежную кассету
  def addCashTape(bankNoteRating:Int) = {
    cashTapes += CashTape(bankNoteRating)
  }

  //Методы для нахождения максимальной и минимальной номианалов валют
  def ratingMax = {val max = cashTapes.maxBy(r => r.bankNoteRating); max.bankNoteRating}
  def ratingMin = {val min = cashTapes.minBy(r => r.bankNoteRating); min.bankNoteRating}

  //Cумма всей налчики со всех денежных кассет
  def getTotalCash : Int = {
    var total: Int = 0
    for (tape <- cashTapes) total += tape.getTotal
    total
  }

  //посмотреть колличество денежных кассет у банкомата
  def printAmountCashTapes : Unit = {
    println(cashTapes.length)
  }

  //метод для нахожденя денежной кассеты по номиналу
  private def findCashTapeByRating(rating : Int) : Option[CashTape] = {
    var cashTape :Option[CashTape] = None
    for (tape <- cashTapes if tape.bankNoteRating.equals(rating)) cashTape = Some(tape)
    cashTape
  }

  //пополнение денежной кассеты
  def refillCashTape(rating: Int, amount: Int) = {
    val cashTapeOption :Option[CashTape] = findCashTapeByRating(rating)
    cashTapeOption foreach {
      tape => tape.refill(amount)
    }
  }

  //получить деньги из денежной кассеты
  def withdrawCashTape(rating: Int, amount: Int) = {
    val cashTapeOption :Option[CashTape] = findCashTapeByRating(rating)
    cashTapeOption foreach {
      tape => tape.withdrawBankNotes(amount)
    }
  }

  //вывести номиналы валют и их колличесвто в банкомате
  def printRatingAndAmountBankNotes : Unit = {
   val tapes : ArrayBuffer[CashTape] = cashTapes.sortBy(r => r.bankNoteRating).reverse
   for (tape <- tapes) println(tape.bankNoteRating,tape.getAmount)
  }

  //метод для обработки цепочки выдачи денег
  private def cashTapeHandler (rating: Int, nextTapeByRating: Int) = {
    //Проверям есть ли денежная кассета с таким наминалом.
    val cashTapeOption :Option[CashTape] = findCashTapeByRating(rating)
    //Проходим по "опции"
    cashTapeOption foreach {
      //Если есть такая кассета то:
      tape => {
        //узнаем сколько может дать банкомат валюты данного номинала
        val bankNotesAmount  : Int = tape.howMuchBankNotesICanGive(cash)
        //получаем деньги из денежной кассеты данного номинала
        tape.withdrawBankNotes(bankNotesAmount)
        //Вычитаем из cash деньги которы мы взяли с денежнной кассеты
        cash     = cash - ( bankNotesAmount * tape.bankNoteRating)
        //Назначаем следующий обработчик, по номиналу
        val next = Event(nextTapeByRating)
        //Вызываем частичную функцию, в данном контексте цепочку
        cashEvent(next)
      }
    }
  }

  //Функция, определенная на подмножестве номиналов
  def cashRubHandler(amountCash: Int): EventHandler = {
    case Event(5000) =>  cashTapeHandler(5000, 1000)
    case Event(1000) =>  cashTapeHandler(1000, 500 )
    case Event(500)  =>  cashTapeHandler(500 , 100 )
    case Event(100)  =>  cashTapeHandler(100 , 50  )
  }

  //получаем деньги из банкомата
  def windthrawCash(cashAmount:Int)  = {
    //события начинается с 5000 номинала
    val head = Event(5000)
   //присваиваем cash сумму которую хотим снять
    cash = cashAmount
    //если мы способны вообще дать эту наличность, то проходим по цепочке
    if (getTotalCash >= cashAmount) cashEvent(head)
  }

  //Функция, определенная на подмножестве номиналов, отличие
  //состоит в том, что это функция для мелких купюр
  def pettyCashRubHandler(amountCash: Int): EventHandler = {
    case Event(50)   =>  cashTapeHandler(50  , 100 )
    case Event(100)  =>  cashTapeHandler(100 , 500 )
    case Event(500)  =>  cashTapeHandler(500 , 1000)
    case Event(1000) =>  cashTapeHandler(1000, 5000)
  }

  def windthrawPettyCash(cashAmount:Int) = {
    //события начинается с 50 номинала
    val head = Event(50)
    //присваиваем cash сумму которую хотим снять
    cash = cashAmount
    //если мы способны вообще дать эту наличность, то проходим по цепочке
    if (getTotalCash >= cashAmount) cashEvent(head)
  }
}

//Объект-компаньон
object ATM {
  // метод apply - для удобсвта создания экземпляров класса ATM
  def apply() = new ATM
}
