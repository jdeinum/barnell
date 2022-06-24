import scala.collection.immutable
import scala.collection.mutable
import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
import scala.sys
import Console.{GREEN, RED, BLUE, RESET, YELLOW, UNDERLINED, BOLD}
import util.control.Breaks._

// Main Driver Function
@main def mainLoop: Unit = 
  var input = 0
  while true do
    breakable {
      println("----------------------------------------------------------------")
      println("""Select An Option:
      1) Enter the questions the student got correctly
      2) Enter the questions the student got incorrectly
      3) Exit""")
      print("> ")
      try
        input = readLine().strip().toInt
      catch 
        case e: NumberFormatException => {break}

      input match {
        case 1 => calc(false)
        case 2 => calc(true)
        case 3 => {println("Have a great day!"); sys.exit(0) }
      }
    }

def calc(reverse: Boolean): Unit =
  val list = getNumberList()
  val correct = getCorrectList(list, reverse)
  val scores = calculateScore(correct)
  if scores.toList.length == 0 then 
    return
  printScores(scores)


// Read in a list of numbers from the user
def getNumberList(): List[Int] =
  print("Enter numbers seperated by spaces:  ")
  val input = readLine()
  try
    val line = input.split(" ").filter(x => x != "")
    line.foreach(x => x.strip())
    val integers = line.map(t => t.toInt).toSet.toList
    return integers

  catch 
    case e: NumberFormatException => {
      Console.println(s"${RESET}${RED}${BOLD}There was an error with your input!${RESET}")
      List()
    }


// get the list of questions the student entered correctly
def getCorrectList(numbers: List[Int], reverse: Boolean): List[Int] =
  reverse match {
    case false => numbers
    case true => getInverse(numbers)
  }


// gets the set difference
def getInverse(numbers: List[Int]): List[Int] = 
  val possible: List[Int] = List.range(1, 101)
  return possible.diff(numbers)



// Calculate the score for a student
def calculateScore(numbers: List[Int]): Map[Char, String] = 
  if numbers.length == 0 then
    return Map()
  val grouped = questions.map((key, value) => (key, numbers.intersect(value).length))
  val levels = grouped.map((a,b) => (a, getLevel(a, b)))
  return levels


def getLevel(category: Char, correct: Int): String = 
  val scores = score.get(category).getOrElse(List())
  var x = 0
  while x < scores.length - 1 do
    if correct >= scores(x) && scores(x) > 0 then
      return levels(x)
    
    x += 1
  
  return levels(x)


  

def printScores(scores: Map[Char, String]): Unit =
  Console.println(s"\n\n${RESET}${BLUE}${BOLD}RESULTS:${RESET}")
  scores.toSeq.sortBy(_._1).foreach((key, value) => prettyPrint(key, value))
  print("\n\n\n\n\n\n")
    


def prettyPrint(key: Char, v: Any): Unit =
  v match {
    case "NI" => Console.println(s"${RESET}$key -> ${RED}${BOLD}$v${RESET}")
    case "ID" => Console.println(s"${RESET}$key -> ${GREEN}${BOLD}$v${RESET}")
    case "NR" => Console.println(s"${RESET}$key -> ${YELLOW}${BOLD}$v${RESET}")
  }



val questions = immutable.Map (
  'A'-> List(1,2,3,4,5),
  'B'-> List(6,7,8),
  'C'-> List(9,10,11,12),
  'D'-> List(13,14,15,16,17,18),
  'E'-> List(19,20,21,22),
  'F'-> List(23,24,25,26,27),
  'G'-> List(28,29,30,31,32,33,34),
  'H'-> List(35,36,37),
  'I'-> List(38,39,40),
  'J'-> List(41,42,43),
  'K'-> List(44,45,46),
  'L'-> List(47,48,49),
  'M'-> List(50,51,52),
  'N'-> List(53,54,55),
  'O'-> List(56,57,58,59),
  'P'-> List(60,61,62,63),
  'Q'-> List(64,65,66,67),
  'R'-> List.range(68, 88),
  'S'-> List.range(88, 101))


val count: Map[Char, Int] = questions.map((key, value) => (key, value.length)).toMap

val levels = immutable.List("ID","NR","NI")

val score: Map[Char, List[Int]] = immutable.Map (
  'A'-> List(5,4,3),
  'B'-> List(3,-1,2),
  'C'-> List(4,3,2),
  'D'-> List(6,5,4),
  'E'-> List(4,3,2),
  'F'-> List(5,4,3),
  'G'-> List(7,6,4),
  'H'-> List(3,-1,2),
  'I'-> List(3,-1,2),
  'J'-> List(3,-1,2),
  'K'-> List(3,-1,2),
  'L'-> List(3,-1,2),
  'M'-> List(3,-1,2),
  'N'-> List(3,-1,2),
  'O'-> List(4,3,2),
  'P'-> List(4,3,2),
  'Q'-> List(4,3,2),
  'R'-> List(20,17,15),
  'S'-> List(13,11,9))






