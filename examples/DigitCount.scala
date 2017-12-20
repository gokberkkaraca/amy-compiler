object DigitCount extends App {

  def triangle(row: Int): Int = {
    if (row < 0) {
      error("There is no triangle to count the blocks")
    }
    else {
      if (row == 0) {
        0
      }
      else {
        row + triangle(row - 1)
      }
    }
  }

  def count(num: Int, digit: Int, acc:Int, prev: Int, rowOfDigit: Boolean): Int = {
    if (num < 0) {
      error("Negative numbers are not accepted")
    }
    else {
      if (num == 0) {
        acc
      }
      else {
        if (rowOfDigit) {
          if (num % 10 == digit) {
            count(num / 10, digit, acc, num % 10, true)
          }
          else {
            count(num / 10, digit, acc, num % 10, false)
          }
        }
        else {
          if (num % 10 == digit) {
            if (prev == digit) {
              count(num / 10, digit, acc - 1, num % 10, true)
            }
            else {
              count(num / 10, digit, acc + 1, num % 10, false)
            }
          }
          else {
            count(num / 10, digit, acc, num % 10, false)
          }
        }
      }
    }
  }

  def digitCounter(number: Int, digit: Int): Int ={
    count(number, digit, 0, -1, false)
  }


  Std.printString("Number of blocks in a 5 row triangle is: " ++ Std.intToString(triangle(5)));
  Std.printString("Number of blocks in a 4 row triangle is: " ++ Std.intToString(triangle(4)));
  Std.printString("Number of blocks in a 3 row triangle is: " ++ Std.intToString(triangle(3)));
  Std.printString("Number of blocks in a 2 row triangle is: " ++ Std.intToString(triangle(2)));
  Std.printString("Number of blocks in a 1 row triangle is: " ++ Std.intToString(triangle(1)));
  Std.printString("Number of blocks in a 0 row triangle is: " ++ Std.intToString(triangle(0)));

  Std.printInt(digitCounter(551555, 5)); // 0
  Std.printInt(digitCounter(81881811, 8)); // 2
  Std.printInt(digitCounter(81181811, 1)); // 1
  Std.printInt(digitCounter(27272272, 2)) // 3
}