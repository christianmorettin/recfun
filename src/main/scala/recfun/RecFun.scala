package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0 )
        throw new RuntimeException("Le colonne e le righe passate devono essere maggiori di zero")
    else{
      if (c == 0 || r==0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def parBalance(chars: List[Char], parOpen: Int): Boolean = {
      if (parOpen < 0){
        //ci sono più parentesi chiuse che aperte
        false
      }else
      if (chars.isEmpty) {
        parOpen == 0
      }else
      if (chars.head == '(') {
        parBalance(chars.tail, parOpen + 1)
      } else
      if (chars.head == ')') {
        parBalance(chars.tail, parOpen - 1)
      }
      else{
        parBalance(chars.tail, parOpen)
      }

    }

    parBalance(chars, 0)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) {
      0
    } else if (money == 0){
      1
    }
    else countChange(money - coins.head, coins) + countChange(money , coins.tail)
  }
}
