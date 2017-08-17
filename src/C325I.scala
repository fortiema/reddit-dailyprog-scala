import scala.collection.mutable.PriorityQueue

case class Step(depth: Int, posCurr: (Int, Int), path: Set[(Int, Int)], visited: Set[(Int, Int)])
  extends Ordered[Step] {

  def compare(that: Step): Int = this.depth compare that.depth
}

object C325I extends App {
  // Read in starting pos
  import scala.io.StdIn

  val startPos: Array[Int] = StdIn.readLine().split(",").map(t => t.stripPrefix("(").stripSuffix(")").toInt)

  // While maze info, read info + build maze
  var maze = Array.ofDim[Set[(Int, Int)]](10,10)
  var data = StdIn.readLine()
  var exit = (0,0)
  var i = 0
  var dim = 10
  while(data != null && data != "") {
    // set appropriate dimension limit to calculate offlimit steps later
    if (i == 0) dim = data.split(" ").length
    for ((t, j) <- data.split(" ").zipWithIndex) {
      if (maze(i)(j) == null) maze(i)(j) = Set[(Int,Int)]()
      t match {
        case n  if t == "n" => maze(i)(j) += Tuple2(i-1, j)
        case ne if t == "ne" => maze(i)(j) += Tuple2(i-1, j+1)
        case nw if t == "nw" => maze(i)(j) += Tuple2(i-1, j-1)
        case s  if t == "s" => maze(i)(j) += Tuple2(i+1, j)
        case se if t == "se" => maze(i)(j) += Tuple2(i+1, j+1)
        case sw if t == "sw" => maze(i)(j) += Tuple2(i+1, j-1)
        case w  if t == "w" => maze(i)(j) += Tuple2(i, j-1)
        case e  if t == "e" => maze(i)(j) += Tuple2(i, j+1)
        case h  if t == "h" => exit = (i, j)
      }
    }
    i += 1
    data = StdIn.readLine()
  }

  val solution = dfs(maze, (startPos(0), startPos(1)), exit)
  println(solution.mkString("\n"))

  // Depth First Search
  // Adapted from: https://medium.com/@lennyboyatzis/ai-teaching-pacman-to-search-with-depth-first-search-ee57daf889ab
  def dfs(graph: Array[Array[Set[(Int, Int)]]],
          start: (Int, Int),
          goal: (Int, Int)): Set[(Int, Int)] = {

    var visited = Set[(Int, Int)]()
    var path = Set[(Int, Int)]()

    // Reverse order sort: https://stackoverflow.com/a/7803042/754581
    var fringe = PriorityQueue.empty[Step](Ordering[Step].reverse)
    fringe += Step(0, start, Set[(Int, Int)](), Set[(Int, Int)]())

    while (fringe.nonEmpty) {

      // Convert case class into tuple - http://michalostruszka.pl/blog/2015/03/30/scala-case-classes-to-and-from-tuples/
      var (depth, current_node, path, visited) = Step.unapply(fringe.dequeue()).get

      if (current_node == goal) return {path + current_node}

      visited += current_node

      var child_nodes: Set[(Int, Int)] = graph(current_node._1)(current_node._2)

      // Need to add the option of continuing on the same path instead of changing direction...
      if (path.size > 1) {
        val xStep = current_node._1 + (current_node._1 - path.last._1)
        val yStep = current_node._2 + (current_node._2 - path.last._2)
        if (xStep < dim || yStep < dim) child_nodes += Tuple2(xStep, yStep)
      }

      for (node <- child_nodes.toIterator) {
        if (!visited.contains(node)) {
          if (node == goal) return {path + node}
          fringe += Step(-path.size, node, path + node, visited + node)
        }
      }
    }
    path
  }
}
