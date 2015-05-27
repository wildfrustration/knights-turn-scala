
object All_open_tours_from_position_single_thread extends App {

  def allTours(x: Int, y: Int, route: List[(Int, Int)]): List[List[(Int, Int)]] = {
    if(route.size == (n * n)) {
      route :: Utils.LegalMoves(x, y, n).filter(move => !route.contains(move)).map { case move @ (x, y) =>
        allTours(x, y, route :+ (x,y))
      }.flatten
    } else {
      Utils.LegalMoves(x, y, n).filter(move => !route.contains(move)).map { case move @ (x, y) =>
        allTours(x, y, route :+ (x,y))
      }.flatten
    }
  }

  print("Enter N for board with size N x N: ")
  val n: Int = io.StdIn.readInt

  println(s"Creating board $n by $n...")

  print(s"Enter starting column (1 - $n): ")
  val beginX: Int = io.StdIn.readInt - 1

  print(s"Enter starting row (1 - $n): ")
  val beginY: Int = io.StdIn.readInt - 1

  val solutions = allTours(beginX, beginY, List((beginX, beginY)))

  println(solutions.size + " solutions found, map through the solutions list to see them")

}


object One_open_tour_from_position_single_thread extends App {

  var stop: Boolean = false

  def firstTourFound(x: Int, y: Int, route: List[(Int, Int)]): Option[List[(Int, Int)]] = {

    if(route.size == (n * n)) {
      stop = true
      return Some(route)
    }

    Utils.LegalMoves(x, y, n).filter(move => !route.contains(move)).flatMap { case move @ (x, y) =>
      if(stop) None else firstTourFound(x, y, route :+ (x,y))
    }.headOption
  }

  print("Enter N for board with size N x N: ")
  val n: Int = io.StdIn.readInt

  println(s"Creating board $n by $n...")

  print(s"Enter starting column (1 - $n): ")
  val beginX: Int = io.StdIn.readInt - 1

  print(s"Enter starting row (1 - $n): ")
  val beginY: Int = io.StdIn.readInt - 1

  firstTourFound(beginX, beginY, List((beginX, beginY))).map { solution =>
    println("solution found: " + solution)
  } getOrElse {
    println("can't find solution")
  }

}


object Utils {

  //give us possible moves inside the board without knowing which of the positions were used already
  def LegalMoves(x: Int, y: Int, n: Int) = Set (
    (x - 1, y - 2),
    (x - 1, y + 2),
    (x - 2, y - 1),
    (x - 2, y + 1),
    (x + 1, y - 2),
    (x + 1, y + 2),
    (x + 2, y - 1),
    (x + 2, y + 1)
  ).filter { case (x, y) => //using case to avoid _1 and _2
    x >= 0 && y >= 0 && x < n && y < n //filter the ones that will end up outside the board
  }.toList

}