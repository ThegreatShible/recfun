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
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case(0,_) => 1
    case (x,y) if x == y => 1
    case (x,y) => pascal(c,r-1) + pascal (c-1,r-1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def innerBalance(chars : List[Char], openParanthese : Int) : Boolean = (chars , openParanthese) match {
      case (Nil , x ) => if(x == 0) true else false
      case (_ , x) if x < 0 => false
      case ( '(' :: y , z ) => innerBalance(y , z+1)
      case ( ')' :: y , z ) => innerBalance(y , z-1)
      case (x :: y , z ) => innerBalance(y,z)
    }

    innerBalance(chars,0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = (money , coins) match {
    case (0, _) => 1
    case (x, _) if x < 0 => 0
    case (_ , Nil) => 0
    case (x , y) => countChange(x-y.head , y) + countChange(x, y.tail)
  }
}
