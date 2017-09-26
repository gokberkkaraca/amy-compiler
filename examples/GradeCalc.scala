object GradeCalc extends App {

  def weightedTotal(quizzes: Int, midtermExam: Int, finalExam: Int): Int = {
    (quizzes * 2) / 10 + (midtermExam * 3) / 10 + (finalExam * 5) / 10
  }

  def calc(quizzes: Int, midtermExam: Int, finalExam: Int): String = {

    weightedTotal(quizzes, midtermExam, finalExam) match {
      case 10 => "A"
      case 9 => "B"
      case 8 => "C"
      case 7 => "D"
      case 6 => "E"
      case 5 => "F"
      case 4 => "G"
      case 3 => "H"
      case 2 => "I"
      case 1 => "J"
      case 0 => "K"
    }
  }

  Std.printString(calc(10, 10, 10));
  Std.printString(calc(7, 4, 10));
  Std.printString(calc(5, 7, 7))
}