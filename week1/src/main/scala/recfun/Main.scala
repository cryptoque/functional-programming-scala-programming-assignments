package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      def pascalIter(c:Int, r:Int): Int = {
        if(r==0) 1
        else if(c==0 || c==r ) {
          pascalIter(c, r-1)
        }
        else pascalIter(c-1, r-1)+ pascalIter(c,r-1)

      }
      pascalIter(c, r)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceInner(chars:List[Char], balance:Int):Boolean ={
        // println(balance)
        // println(chars.head)

        if(balance<0) {false}
        else if(chars.isEmpty && balance == 0) {true}
        else if(chars.isEmpty && balance !=0) {false}
        else if (!chars.isEmpty && chars.head == '(')  {balanceInner(chars.tail, balance +1)}
        else if (!chars.isEmpty && chars.head == ')') {balanceInner(chars.tail, balance -1)}
        else {balanceInner(chars.tail, balance)}
        }
      balanceInner(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if(money > 0 && !coins.isEmpty) countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else 0



    }
  }